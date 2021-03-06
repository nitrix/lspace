{-# LANGUAGE ConstraintKinds #-}

-- TODO: Maybe use TVar instead of IORef to allow concurrency ?

module Link
    ( Link
    , Linkable

    , Context
    , initContext
    , saveContext

    , defaultLink
    , invalidLink
    , createLink
    , destroyLink
    , restoreLink
        
    , readLink
    , writeLink
    , modifyLink
    )
    where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Cache.LRU as L
import Data.Dynamic
import Data.Foldable
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import System.Mem
import System.Mem.Weak
import System.Directory
import System.IO.Error
import System.IO.Unsafe

-- TODO: instead of IO a, please use MonadIO m => m a

type Linkable a = (Typeable a, FromJSON a, ToJSON a)

type LinkId = Integer
type LinkRef a = IORef (Maybe (Weak (IORef a)))
type LinkCache = L.LRU LinkId LinkCacheWrapper

data LinkCacheWrapper = MkLinkCacheWrapper
    { lcwDynamic  :: Dynamic
    , lcwSaveLink :: IO ()
    }

-- TODO: This should contain a linkRead, linkUpdate... to get rid of unsafePerformIO
-- It should incidentally fix the GHCi reload problems where there's previous references bugging out not being collected
data Link a = MkLink
    { linkId  :: LinkId
    , linkRef :: LinkRef a
    }

data Context = MkContext -- TODO: needs a better name than that
    { ctxCache      :: IORef LinkCache
    , ctxJsonStore  :: FilePath
    , ctxNextLinkId :: Link LinkId
    }

instance Eq (Link a) where
    (==) (MkLink i _) (MkLink k _) = i == k

instance Ord (Link a) where
    compare (MkLink i _) (MkLink k _) = i `compare` k

instance Show (Link a) where
    show (MkLink i _) = "{Link #" ++ show i ++ "}"

instance Linkable a => FromJSON (Link a) where
    parseJSON (String i) = return . restoreLink . read . T.unpack $ i
    parseJSON _ = error "Unable to parse Link json"

instance ToJSON (Link a) where
    toJSON (MkLink i _) = String . T.pack . show $ i

-- TODO: Functor, Applicative, Monad instances of `Relational` newtype,
-- then give it a MonadRelational and allow it to be newtypederived for Game.
-- This might prompt a huge refactor of the functions below.

-- | Initializes a context for link isolation and preservation.
initContext :: Maybe Integer -> FilePath -> IO Context
initContext maybeLimit jsonStore = do
    cache <- newIORef (L.newLRU maybeLimit)
    return $ MkContext cache jsonStore (restoreLink 0)

-- | Default link
defaultLink :: Link a
defaultLink = restoreLink 1

-- | Invalid link
invalidLink :: Link a
invalidLink = restoreLink (-1)

-- | Linkify something
createLink :: Linkable a => Context -> a -> IO (Link a)
createLink ctx x = do
    refVal     <- newIORef x
    weakRefVal <- mkWeakIORef refVal (return ())
    ref        <- newIORef $ Just weakRefVal
    maybeLid   <- readLink ctx (ctxNextLinkId ctx)
    
    case maybeLid of
        Just _ -> modifyLink ctx (ctxNextLinkId ctx) (+1)
        Nothing -> do
            -- TODO review this case here, when there's no next id
            lidVal     <- newIORef 2
            weakLidVal <- mkWeakIORef lidVal (return ())
            writeIORef (linkRef $ ctxNextLinkId ctx) (Just weakLidVal)
            saveLink ctx (ctxNextLinkId ctx)
            void $ fixLink ctx (ctxNextLinkId ctx)
    
    let link = MkLink (fromMaybe 1 maybeLid) ref
    
    -- TODO: instead of this, we could manually add it to the cache and fix the link, to not touch the disk
    saveLink ctx link
    _ <- fixLink ctx link
    
    return link

-- | Destroy a link
destroyLink :: Context -> Link a -> IO ()
destroyLink ctx link = do
    modifyIORef' (ctxCache ctx) $ \cache -> fst (L.delete (linkId link) cache)
    removeFile filename
    modifyIORef' (linkRef link) (const Nothing)
    where
        filename = ctxJsonStore ctx ++ show (linkId link) ++ ".json"
    
-- | This creates an unresolved link, any LinkId is therefore valid and doesn't require IO.
restoreLink :: LinkId -> Link a
restoreLink lid = unsafePerformIO $ do -- Yes, I know what I'm doing.
    ref <- newIORef Nothing
    return $ MkLink lid ref
    
-- | Writes a given value to a link
writeLink :: Linkable a => Context -> Link a -> a -> IO ()
writeLink ctx link x = modifyLink ctx link (const x)

-- | Transform the value of a link by a given function
modifyLink :: Linkable a => Context -> Link a -> (a -> a) -> IO ()
modifyLink ctx link f = do
    maybeWeakRefVal <- readIORef (linkRef link)
    case maybeWeakRefVal of
        -- The link has already been resolved and we have a weak pointer in memory.
        Just weakRefVal -> do
            maybeRefVal <- deRefWeak weakRefVal
            case maybeRefVal of
                -- Our weak pointer is still valid, use that.
                Just refVal -> modifyIORef' refVal f
                -- Unfortunately, the weak pointer was garbage collected; so we need to reload the link again.
                Nothing -> do
                    maybeVal <- fixLink ctx link
                    case maybeVal of
                        -- It failed to load the link, we cannot modify anything then.
                        Nothing -> return ()
                        -- It recovered the link successfully; let's try again
                        Just _ -> modifyLink ctx link f
        -- The link is not resolved yet
        Nothing -> do
            maybeVal <- fixLink ctx link
            case maybeVal of
                -- It failed to load the link, we cannot modify anything then.
                Nothing -> return ()
                -- It resolved the link successfully; let's try again
                Just _ -> modifyLink ctx link f

-- | Reads the value of a link
readLink :: Linkable a => Context -> Link a -> IO (Maybe a)
readLink ctx link = do
    maybeWeakRefVal <- readIORef (linkRef link)
    case maybeWeakRefVal of
        -- The link has already been resolved and we have a weak pointer in memory.
        Just weakRefVal -> do
            maybeRefVal <- deRefWeak weakRefVal
            case maybeRefVal of
                -- Our weak pointer is still valid, use that.
                Just refVal -> do
                    val <- readIORef refVal
                    return $ Just val
                -- Unfortunately, the weak pointer was garbage collected; so we need to reload the link again.
                Nothing -> fixLink ctx link
        -- The link is not resolved yet
        Nothing -> do
            cache <- readIORef (ctxCache ctx)
            -- Let's try to resolve it from the cache
            let (_, maybeCached) = L.lookup (linkId link) cache
            case maybeCached of
                -- Not found in cache; time to reload the link
                Nothing -> fixLink ctx link
                -- We have it cache, use that.
                Just cached -> do
                    case fromDynamic (lcwDynamic cached) of
                        Nothing -> return Nothing
                        Just refVal -> do
                            val <- readIORef refVal
                            return $ Just val

-- | This function loads a value in cache and create a healthy resolved link to the cache entry.
fixLink :: Linkable a => Context -> Link a -> IO (Maybe a)
fixLink ctx link = do
    cache <- readIORef (ctxCache ctx)

    -- Make sure the entry doesn't already exist in cache first
    let (newCache, maybeCached) = L.lookup (linkId link) cache

    case maybeCached of
        Just cached -> do
            modifyIORef (ctxCache ctx) (const newCache)
            case (fromDynamic $ lcwDynamic cached) of
                -- The value present in cache has been incorrectly cast; ignoring the user mistake to preserve stability
                Nothing -> return Nothing 
                -- Recovered former cache entry, using that
                Just refVal -> do
                    newWeakRefVal <- mkWeakIORef refVal (return ())
                    writeIORef (linkRef link) (Just newWeakRefVal)
                    readLink ctx link
        
        Nothing -> do
            newMaybeVal <- loadVal ctx (linkId link)
            case newMaybeVal of
                -- Unable to load the value
                Nothing -> return Nothing
                Just newVal -> do
                    -- Fixing the link
                    newRefVal <- newIORef newVal
                    newWeakRefVal <- mkWeakIORef newRefVal (return ())
                    writeIORef (linkRef link) (Just newWeakRefVal)
                    
                    -- Caching; here we exploit that saveLink is lazy
                    let (newNewCache, maybeDropped) = L.insertInforming
                                                      (linkId link)
                                                      (MkLinkCacheWrapper (toDyn newRefVal) (saveLink ctx link))
                                                      newCache

                    modifyIORef' (ctxCache ctx) (const newNewCache)
                    fold $ lcwSaveLink . snd <$> maybeDropped

                    return newMaybeVal

-- | Helper function to load values from disk
loadVal :: Linkable a => Context -> LinkId -> IO (Maybe a)
loadVal ctx lid = do
    -- TODO: ignore io exceptions
    -- putStrLn ("Loading link #" ++ show lid)
    result <- eitherDecode' . LB.fromStrict <$> catchIOError (B.readFile filename) (const $ return B.empty)
    case result of
        Right x -> return $ Just x
        Left x -> do
            putStrLn ("Failed to decode #" ++ show lid ++ " with error: " ++ x)
            return Nothing
    where
        filename = ctxJsonStore ctx ++ show lid ++ ".json"

-- | Helper function to save a link to disk.
saveLink :: Linkable a => Context -> Link a -> IO ()
saveLink ctx link = do
    -- putStrLn ("Saving link #" ++ show (linkId link))
    -- TODO: ignore io exceptions
    -- TODO: also create directory automatically if missing
    maybeVal <- readLink ctx link
    case maybeVal of
        Nothing -> return ()
        Just val -> B.writeFile filename $ LB.toStrict $ encode val
    where
        filename = ctxJsonStore ctx ++ show (linkId link) ++ ".json"

-- | Notably saves all links
saveContext :: Context -> IO ()
saveContext ctx = do
    -- putStrLn "Saving context"
    cache <- readIORef (ctxCache ctx)
    mapM_ (lcwSaveLink . snd) (L.toList cache)
    -- TODO: Link is breaking referential transparency. TIL. It needs a good refactoring.
    performMajorGC
