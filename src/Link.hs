{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}

module Link where

import Debug.Trace

import Control.Monad
import Data.Aeson as J
import Data.Dynamic
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Lazy as LB
import System.Directory
import System.Mem.Weak
import System.IO.Unsafe
import qualified Data.Cache.LRU as L

type LinkId = Int
data Link a = MkLink {-# UNPACK #-} !(IORef (LinkId, Maybe (Weak (IORef a))))

type Linked a = (Typeable a, FromJSON a, ToJSON a)

instance Eq (Link a) where
    (==) (MkLink refA) (MkLink refB) = unsafePerformIO $ do
        a <- fst <$> readIORef refA
        b <- fst <$> readIORef refB
        return $ a == b 

instance Ord (Link a) where
    compare (MkLink refA) (MkLink refB) = unsafePerformIO $ do
        a <- fst <$> readIORef refA
        b <- fst <$> readIORef refB
        return $ a `compare` b 

instance Show (Link a) where
    show (MkLink ref) = unsafePerformIO $ do
        (i, _) <- readIORef ref
        return $ "{Link #" ++ show i ++ "}"

instance Linked a => FromJSON (Link a) where
    parseJSON (J.Number n) = do
        return $ getLinkId $ truncate n
    parseJSON _ = error "Unable to parse Link json"

instance ToJSON (Link a) where
    toJSON (MkLink lnk) = Number $ fromIntegral $ fst $ unsafePerformIO $ readIORef lnk

-- TODO: temporary absolutely disgusting and super unsafe
{-# NOINLINE refCache #-}
refCache :: IORef (L.LRU LinkId (Dynamic, IO ()))
refCache = unsafePerformIO (newIORef $ L.newLRU $ Just 1000) -- TODO: option for the amount of links to keep in memory?

createLink :: a -> IO (Link a)
createLink x = do
    trace "createLink" $ do
    ref           <- newIORef x
    weakRef       <- mkWeakIORef ref (return ())
    nextCountLink <- MkLink <$> newIORef (0, Nothing)
    nextCount     <- fromMaybe 1 <$> readLink nextCountLink
    linkRef       <- newIORef (nextCount, Just weakRef)
    modifyLink nextCountLink (+1)
    return $ MkLink linkRef

writeLink :: Linked a => Link a -> a -> IO ()
writeLink link@(MkLink ref) x = do
    trace "writeLink" $ do
    maybeContent <- readLink link
    case maybeContent of
        Nothing -> return ()
        Just _ -> do
            (_, r) <- readIORef ref
            case r of
                Nothing -> return () -- It got invalidated midway?
                Just weakVRef -> do
                    vRef <- deRefWeak weakVRef
                    case vRef of
                        Nothing -> return () -- weird
                        Just v -> writeIORef v x

modifyLink :: Linked a => Link a -> (a -> a) -> IO ()
modifyLink link@(MkLink ref) f = do
    trace "modifyLink" $ do
    content <- readLink link
    (_, maybeWeak) <- readIORef ref
    case content of
        Nothing -> return () -- Broken link, cannot be updated
        Just _  -> do
            case maybeWeak of
                Nothing -> return () -- Broken link, cannot be updated
                Just weak -> do
                    maybeContent <- deRefWeak weak
                    case maybeContent of
                        Nothing   -> return () -- Broken link, cannot be updated
                        Just refY -> do
                            modifyIORef' refY f

getLinkId :: Linked a => Int -> Link a
getLinkId n = unsafePerformIO $ do
    trace "getLinkId" $ do
    links <- readIORef refCache
    let (newLinks, maybeVal) = L.lookup n links
    case maybeVal of
        Just val -> case fromDynamic (fst val) of
            Just r -> do
                weak <- mkWeakIORef r (return ())
                ref <- newIORef (n, Just weak)
                return $ MkLink ref
            Nothing -> error "This cannot happen"
        Nothing -> do
            ref <- newIORef (n, Nothing)
            let (newNewLinks, maybeDropped) = L.insertInforming n (toDyn ref, saveLink $ MkLink ref) newLinks
            
            case maybeDropped of
                Nothing -> return ()
                Just dropped -> snd $ snd dropped
        
            modifyIORef' refCache $ const newNewLinks
            
            return $ MkLink ref

readLink :: forall a. Linked a => Link a -> IO (Maybe a)
readLink (MkLink link) = do
    trace "readLink" $ do
    -- Read the link unsafe bastraction
    (i, r) <- readIORef link
    case r of
        -- Determines if we have a link reference
        Nothing -> do
            result <- loadFreshLinkId i
            case result of
                Nothing -> return Nothing
                Just a  -> do
                    writeIORef link a
                    -- modifyIORef' link $ const a
                    readLink $ MkLink link
        Just x  -> do
            final <- deRefWeak x
            -- Does the reference point to something that still exists
            case final of
                Just z  -> do
                    v <- readIORef z
                    return $ Just v
                Nothing -> do
                    result <- loadFreshLinkId i
                    case result of
                        Nothing -> return Nothing
                        Just a  -> do
                            -- writeIORef link a
                            modifyIORef' link $ const a
                            readLink $ MkLink link
    where
        loadFreshLinkId :: Int -> IO (Maybe (Int, Maybe (Weak (IORef a))))
        loadFreshLinkId i = do
            trace "loadFreshLinkId" $ do
            ok <- doesFileExist filepath
            if ok
            then do
                content <- LB.readFile filepath
                case J.decode content of
                    Nothing -> do
                        return Nothing
                    Just d  -> do
                        ref <- newIORef d

                        links <- readIORef refCache
                        
                        let (newLinks, maybeDropped) = L.insertInforming i (toDyn ref, saveLink (getLinkId i :: Link a)) links
                        case maybeDropped of
                            Nothing -> return ()
                            Just dropped -> snd $ snd dropped

                        modifyIORef' refCache $ const newLinks
                            
                        weakRef <- mkWeakIORef ref (return ())
                        return . Just $ (i, Just weakRef)
            else do
                return Nothing
            where
                filepath = "data/demo/" ++ show i ++ ".json" -- TODO: This has to be fixed
     
--data Link a = MkLink {-# UNPACK #-} !(IORef (LinkId, Maybe (Weak (IORef a))))
saveLink :: Linked a => Link a -> IO ()
saveLink (MkLink ref) = do
    trace "saveLink" $ do
    (i, mc) <- readIORef ref
    case mc of
        Nothing -> do
            return () -- Nothing to save then
        Just c -> do
            z <- deRefWeak c
            case z of
                Nothing -> do
                    return ()
                Just val -> do
                    final <- readIORef val
                    let filepath = "data/demo/" ++ show i ++ ".json" -- TODO: This has to be fixed
                    LB.writeFile filepath $ J.encode final

-- TODO and unload them would be neat
saveAllLinks :: IO ()
saveAllLinks = do
    trace "saveAllLinks" $ do
    links <- map snd . L.toList <$> readIORef refCache
    forM_ links $ \link -> snd link
