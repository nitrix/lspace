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

--trace :: a -> b -> b
--trace _ x = x

-- TODO: Could even replace the LRU with an array

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

instance Linked a => FromJSON (Link a) where
    parseJSON (J.Number n) = do
        return $ getLinkId $ truncate n
    parseJSON _ = error "Unable to parse Link json"

instance ToJSON (Link a) where
    toJSON (MkLink lnk) = Number $ fromIntegral $ fst $ unsafePerformIO $ readIORef lnk

-- New %~ lens combinator that's strict (thanks to puregreen)
-- data Id a = Id {runId :: !a} deriving Functor
-- (%~!) :: ((a -> Id b) -> c -> Id d) -> (a -> b) -> c -> d
-- l %~! f = runId . l (Id . f)

-- TODO: temporary absolutely disgusting and super unsafe
{-# NOINLINE refCache #-}
refCache :: IORef (L.LRU LinkId (Dynamic, IO ()))
refCache = unsafePerformIO (newIORef $ L.newLRU $ Just 1000) -- TODO: option?

createLink :: a -> IO (Link a)
createLink x = do
    ref           <- newIORef x
    weakRef       <- mkWeakIORef ref (return ())
    nextCountLink <- MkLink <$> newIORef (0, Nothing)
    nextCount     <- fromMaybe 1 <$> readLink nextCountLink
    trace ("Creating link #" ++ show nextCount) $ do
    linkRef       <- newIORef (nextCount, Just weakRef)
    modifyLink nextCountLink (+1)
    return $ MkLink linkRef

writeLink :: Linked a => Link a -> a -> IO ()
writeLink link@(MkLink ref) x = do
    (i, _) <- readIORef ref
    trace ("Writing to link #" ++ show i) $ do
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

-- TODO: bogus
modifyLink :: Linked a => Link a -> (a -> a) -> IO ()
modifyLink link@(MkLink ref) f = do
    (i, _) <- readIORef ref
    trace ("Modifying link #" ++ show i) $ do
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
    trace ("Getting link #" ++ show n) $ do
    links <- readIORef refCache
    let (newLinks, maybeVal) = L.lookup n links
    case maybeVal of
        Just val -> case fromDynamic (fst val) of
            Just r -> do
                trace ("Found, re-using that link") $ do
                weak <- mkWeakIORef r (return ())
                ref <- newIORef (n, Just weak)
                return $ MkLink ref
            Nothing -> error "This cannot happen"
        Nothing -> do
            trace ("Not found, creating that link") $ do
            ref <- newIORef (n, Nothing)
            let (newNewLinks, _) = L.insertInforming n (toDyn ref, saveLink $ MkLink ref) newLinks
            modifyIORef' refCache $ const newNewLinks
            -- TODO, the _ is maybeDropped
            return $ MkLink ref

readLink :: forall a. Linked a => Link a -> IO (Maybe a)
readLink (MkLink link) = do
    -- Read the link unsafe bastraction
    (i, r) <- readIORef link
    trace ("Reading link #" ++ show i) $ do
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
            trace "Loading fresh link" $ do
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
saveLink link@(MkLink ref) = do
    (i, mc) <- readIORef ref
    trace ("Saving link #" ++ show i) $ do
    case mc of
        Nothing -> do
            trace "Nothing to do" $ do
            return () -- Nothing to save then
        Just c -> do
            z <- deRefWeak c
            case z of
                Nothing -> do
                    trace "Lost pointer" $ do
                    return ()
                Just val -> do
                    trace "Saving!" $ do
                    final <- readIORef val
                    let filepath = "data/demo/" ++ show i ++ ".json" -- TODO: This has to be fixed
                    LB.writeFile filepath $ J.encode final

saveAllLinks :: IO ()
saveAllLinks = do
    links <- map snd . L.toList <$> readIORef refCache
    forM_ links $ \link -> snd link
