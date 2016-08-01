{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module Link where

import Debug.Trace

import Data.Aeson as J
import Data.Maybe
import Data.IORef
import Data.Sequence as S
import qualified Data.ByteString.Lazy as LB
import System.Directory
import System.Mem.Weak
import System.IO.Unsafe

data Link a = MkLink {-# UNPACK #-} !(IORef (Int, Maybe (Weak (IORef a))))

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

instance FromJSON (Link a) where
    parseJSON (J.Number n) = do
        return $ MkLink $ unsafePerformIO $ newIORef $ (truncate n, Nothing)
    parseJSON _ = error "Unable to parse Link json"

instance ToJSON (Link a) where
    toJSON (MkLink lnk) = Number $ fromIntegral $ fst $ unsafePerformIO $ readIORef lnk

-- New %~ lens combinator that's strict (thanks to puregreen)
-- data Id a = Id {runId :: !a} deriving Functor
-- (%~!) :: ((a -> Id b) -> c -> Id d) -> (a -> b) -> c -> d
-- l %~! f = runId . l (Id . f)

data AnyIORef = forall a. MkAnyIORef {-# UNPACK #-} !(IORef a)

-- TODO: temporary absolutely disgusting and super unsafe
{-# NOINLINE refCache #-}
refCache :: IORef (S.Seq AnyIORef)
refCache = unsafePerformIO (newIORef S.empty)

createLink :: a -> IO (Link a)
createLink x = do
    ref           <- newIORef x
    weakRef       <- mkWeakIORef ref (return ())
    nextCountLink <- MkLink <$> newIORef (0, Nothing)
    nextCount     <- fromMaybe 1 <$> readLink nextCountLink
    linkRef       <- newIORef (nextCount, Just weakRef)
    modifyLink nextCountLink (+1)
    return $ MkLink linkRef

writeLink :: J.FromJSON a => Link a -> a -> IO ()
writeLink link@(MkLink ref) x = do
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
modifyLink :: J.FromJSON a => Link a -> (a -> a) -> IO ()
modifyLink link@(MkLink ref) f = do
    trace "a" $ do
    content <- readLink link
    trace "b" $ do
    (_, maybeWeak) <- readIORef ref
    trace "c" $ do
    case content of
        Nothing -> trace "d" $ return () -- Broken link, cannot be updated
        Just _  -> trace "e" $ do
            case maybeWeak of
                Nothing -> trace "f" $ return () -- Broken link, cannot be updated
                Just weak -> trace "g" $ do
                    maybeContent <- deRefWeak weak
                    trace "h" $ do
                    case maybeContent of
                        Nothing   -> trace "i" $ return () -- Broken link, cannot be updated
                        Just refY -> do
                            trace "j" $ do
                            modifyIORef' refY f

getLinkId :: Int -> Link a
getLinkId n = unsafePerformIO $ MkLink <$> newIORef (n, Nothing)

readLink :: forall a. J.FromJSON a => Link a -> IO (Maybe a)
readLink (MkLink link) = do
    -- Read the link unsafe bastraction
    trace "1" $ do
    (i, r) <- readIORef link
    trace "2" $ do
    case r of
        -- Determines if we have a link reference
        Nothing -> do
            trace "3" $ do
            result <- loadFreshLinkId i
            case result of
                Nothing -> trace "4" $ return Nothing
                Just a  -> trace "5" $ do
                    writeIORef link a
                    -- modifyIORef' link $ const a
                    readLink $ MkLink link
        Just x  -> do
            trace "6" $ do
            final <- deRefWeak x
            -- Does the reference point to something that still exists
            case final of
                Just z  -> do
                    trace "7" $ do
                    v <- readIORef z
                    return $ Just v
                Nothing -> do
                    trace "8" $ do
                    result <- loadFreshLinkId i
                    case result of
                        Nothing -> trace "9" $ return Nothing
                        Just a  -> do
                            trace "10" $ do
                            -- writeIORef link a
                            modifyIORef' link $ const a
                            readLink $ MkLink link
    where
        loadFreshLinkId :: Int -> IO (Maybe (Int, Maybe (Weak (IORef a))))
        loadFreshLinkId i = do
            trace ("Loading link #" ++ show i) $ do
            ok <- doesFileExist filepath
            if ok
            then do
                content <- LB.readFile filepath
                case J.decode content of
                    Nothing -> do
                        trace "Decoded failure" $ do
                        return Nothing
                    Just d  -> do
                        trace "Decoded success" $ do
                        ref <- newIORef d

                        modifyIORef' refCache $ \links -> (
                                                if S.length links >= maxLinks
                                                then S.drop 1 links
                                                else links
                                              ) |> MkAnyIORef ref

                        weakRef <- mkWeakIORef ref (return ())
                        return . Just $ (i, Just weakRef)
            else do
                return Nothing
            where
                filepath = "data/demo/" ++ show i ++ ".json" -- TODO: This has to be fixed
                maxLinks = 1000
