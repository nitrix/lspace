{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification  #-}

module Relational
    ( Relation
    , readRelation
    , writeRelation
    , (>.>)
    ) where

import Control.Monad.State
import Control.Monad.Trans
import Data.Array.IO
import Data.Dynamic
import Data.IORef
import qualified Data.Map as M

type Related a = Typeable a -- Constraint kind

type Relation a = (CacheId, CellId)
newtype Relational a = Relational { runRelational :: StateT Storage IO a } deriving (Functor, Applicative, Monad, MonadState Storage, MonadIO)

type DynCell = Dynamic -- Cell a
type Cell a = IORef a
type CellId = Integer
type CacheId = Int
data Storage = Storage
    { storageCache :: IOArray CacheId (CellId, DynCell)
    , storageFull  :: M.Map CellId DynCell
    }

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t  where f ~ Relational, a ~ Relation a', b ~ Relation b', s ~ Cell s'

---- Boring stuff

newCell :: a -> Relational (Cell a)
newCell val = Relational $ lift $ newIORef val

readCell :: Related a => Cell a -> Relational a
readCell cell = Relational $ lift $ readIORef cell

modifyCell ::  Related a => Cell a -> (a -> a) -> Relational ()
modifyCell cell func = Relational $ lift $ modifyIORef cell func

writeCell :: Related a => Cell a -> a -> Relational ()
writeCell cell val = Relational $ lift $ writeIORef cell val

---- Fancy stuff

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
    cell <- resolveRelation relation
    newRelation <- readCell cell
    newCell <- resolveRelation (func newRelation)
    readCell newCell