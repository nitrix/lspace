module Relational.Core
    ( createRelation
    , readRelation
    , updateRelation
    , deleteRelation
    , runRelational
    ) where

import Control.Monad.State
import Data.Dynamic
import qualified Data.HashTable.IO as H

import Relational.Store
import Relational.Types

-- TODO: extract cache logic to its own module?

runRelational :: Store s => s -> Relational s () -> IO ()
runRelational store relational = do
    cache <- H.newSized 1000
    evalStateT (unwrapRelational relational) (Context cache store)
    -- TODO: write all elements in the cache

createRelation :: a -> Relational s (Relation a)
createRelation _ = do
    -- TODO: The store is responsible for keeping the next available relation id
    return (Relation 100)

readRelation :: (Store s, Related a) => Relation a -> Relational s a
readRelation (Relation relationId) = do
    -- TODO: Caching
    
    -- Otherwise, we read from the store
    -- TODO: Add to cache
    store <- gets ctxStore
    (result, newStore) <- relationLiftIO $ runStateT (storeRead relationId) store
    modify (\ctx -> ctx { ctxStore = newStore })
    return result

updateRelation :: (Store s, Related a) => Relation a -> a -> Relational s ()
updateRelation (Relation relationId) val = do
    -- TODO: Update cache
    
    -- TODO: Remove this, we don't need to write to disk on each write. I'm just testing.
    store <- gets ctxStore
    (result, newStore) <- relationLiftIO $ runStateT (storeUpdate relationId val) store
    modify (\ctx -> ctx { ctxStore = newStore })
    return result

deleteRelation :: (Store s, Related a) => Relation a -> Relational s ()
deleteRelation = undefined -- TODO

relationLiftIO :: IO a -> Relational s a
relationLiftIO = Relational . liftIO