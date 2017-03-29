{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Relational
    ( Relation
    , newRelation
    , readRelation
    , writeRelation
    , runRelational
    , (>.>)
    ) where

import Control.Monad.Reader
import Data.Dynamic
import Data.IORef
import qualified Data.Map as M

newtype Relational a = Relational (ReaderT Context IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

type ContextName = String
type Related a = Typeable a -- Constraint kind
type DynRelation = Dynamic -- forall a. IORef (Maybe a)
type Relation a = (RelationId, IORef (IORef (Maybe a)))
type RelationId = Integer

-- We want all the outer-iorefs distributed across the application to point at one of either:
--   (1) A singleton `newIORef Nothing` inner-ioref
--   (2) A common inner-ioref to the same inner ioref
data Context = Context
    { storageContextName       :: ContextName
    , storageRelationsRef      :: IORef (M.Map RelationId DynRelation)
    , storageNextRelationIdRef :: IORef RelationId
    }

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t  where f ~ Relational, a ~ Relation a', b ~ Relation b', s ~ Cell s'



runRelational :: ContextName -> Relational () -> IO ()
runRelational contextName (Relational r) = do
    -- TODO: restore our previous nextRelationId from the context
    relationsRef      <- newIORef M.empty
    nextRelationIdRef <- newIORef 0
    runReaderT r $ Context
        { storageContextName       = contextName
        , storageRelationsRef      = relationsRef
        , storageNextRelationIdRef = nextRelationIdRef
        }

newRelation :: Related a => a -> Relational (Relation a)
newRelation val = Relational $ do
    nextRelationIdRef <- asks storageNextRelationIdRef
    relationsRef      <- asks storageRelationsRef
    
    innerIORef        <- liftIO $ newIORef (Just val)
    outerIORef        <- liftIO $ newIORef innerIORef
    relationId        <- liftIO $ atomicModifyIORef' nextRelationIdRef (\x -> (x + 1, x))
    
    liftIO $ atomicModifyIORef' relationsRef (\x -> (M.insert relationId (toDyn innerIORef) x, ()))
    return (relationId, outerIORef)

readRelation :: Related a => Relation a -> Relational a
readRelation relation@(relationId, outerIORef) = undefined
{- do
    innerIORef <- liftIO $ readIORef outerIORef
    val        <- liftIO $ readIORef innerIORef
    case val of
        Just v  -> return v
        Nothing -> loadRelation relationId >>= readRelation
-}

-- TODO: is this thread safe?
writeRelation :: Related a => Relation a -> a -> Relational ()
writeRelation relation val = undefined
{-
do
    innerIORef   <- liftIO $ readIORef outerIORef
    liftIO $ writeIORef innerIORef (Just val)
-}

{-
resolveRelation :: Relation -> Relational (IORef (Maybe a), a)
resolveRelation (relationId, outerIORef) = do
    innerIORef <- liftIO $ readIORef outerIORef
    val        <- liftIO $ readIORef innerIORef
    case val of
        Just v  -> return (val, v)
        Nothing -> do
            
        
    relations <- liftIO $ readIORef relationsRef
    case M.lookup relationId relations of
        Just innerIORef -> do
            val <- liftIO $ readIORef innerIORef
            case val of
                Just v  -> 
                Nothing ->
        Nothing -> do
            let loaded = undefined -- TODO: readFile ("data/" ++ contextName ++ "/" ++ relationId ++ ".json")
            innerIORef <- liftIO $ newIORef (Just loaded)
            atomicModifyIORef' relationsRef (\x -> (M.insert relationId (toDyn innerIORef) x, ()))
            outerIORef <- liftIO $ newIORef innerIORef
            return (relationId, outerIORef)
-}
 
(>.>) :: (Related a, Related b) => Relation a -> (a -> Relation b) -> Relational b
(>.>) = undefined

{-
-- TODO: When the full store is used because the element in the cache storage wasn't found,
-- we should update the cache storage with said element for the future, but let's do so, such
-- that updates are round-robin (to avoid evicting active cache). Hopefully, reads aren't so
-- interlaced that any cache slot becomes contended and keeps mutating (degrating O(1) into O(log n)).
resolveRelation :: Related a => Relation a -> Relational (Cell a)
resolveRelation (cacheId, cellId) = do
    cache <- gets storageCache
    full  <- gets storageFull
    (cachedCellId, cachedDynCell) <- liftIO $ readArray cache cacheId
    if (cachedCellId == cellId)
    then fixDynCell cachedDynCell
    else case M.lookup cellId full of
        Just dynCell -> fixDynCell dynCell
        Nothing      -> error "Reference used to a cell that doesn't exists anymore"
    where
        fixDynCell :: forall a. Related a => DynCell -> Relational (Cell a)
        fixDynCell dynCell = case fromDynamic dynCell of
            Just cell -> return cell
            Nothing   -> error "Wrong reference type for what is within the cell"
            
readRelation :: Related a => Relation a -> Relational a
readRelation = resolveRelation >=> readCell

modifyRelation :: Related a => Relation a -> (a -> a) -> Relational ()
modifyRelation relation val = do
    cell <- resolveRelation relation
    modifyCell cell val

writeRelation :: Related a => Relation a -> a -> Relational ()
writeRelation relation val = do
    cell <- resolveRelation relation
    writeCell cell val

(>.>) :: (Related a, Related b) => Relation a -> (a -> Relation b) -> Relational b
(>.>) relation func = do
    cell      <- resolveRelation relation
    relation' <- readCell cell
    cell'     <- resolveRelation (func relation')
    readCell  cell'
-}