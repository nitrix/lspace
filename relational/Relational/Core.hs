module Relational.Core
    ( createRelation
    , readRelation
    , updateRelation
    , deleteRelation
    , runRelational
    ) where

import Control.Monad
import Control.Monad.State (runStateT, execStateT, evalStateT, liftIO)
import Data.Label.Monadic (gets, puts)
import Data.Dynamic
import qualified Data.HashTable.IO as H

import Relational.Store
import Relational.Types

-- TODO: extract cache logic to its own module?
-- TODO: needs a way to obtain a massive amount of relations at once, to warm up the cache, for things like ViaNetwork and such
-- TODO: probably want a RelationalT as well?

runRelational :: Store s => s -> Relational s () -> IO ()
runRelational store relational = do
    cache <- H.newSized 1000
    evalStateT (unwrapRelational $ initRelational >> relational >> quitRelational) (Context cache store)
    -- Lost type information for the save
    void $ H.foldM (\s (_, v) -> execStateT (snd v) s) store cache

initRelational :: Store s => Relational s ()
initRelational = do
    store    <- gets ctxStore
    newStore <- relationLiftIO (execStateT storeInit store)
    puts ctxStore newStore

quitRelational :: Store s => Relational s ()
quitRelational = do
    store <- gets ctxStore
    relationLiftIO (evalStateT storeQuit store)

createRelation :: (Store s, Related a) => a -> Relational s (Relation a)
createRelation value = do
    store <- gets ctxStore
    cache <- gets ctxCache
    -- Create in the store
    (relationId, newStore) <- relationLiftIO $ runStateT (storeCreate value) store
    puts ctxStore newStore
    -- And to the cache
    relationLiftIO $ H.insert cache relationId (toDyn value, storeUpdate relationId value)
    -- Yield result
    return (Relation relationId)

readRelation :: (Store s, Related a) => Relation a -> Relational s a
readRelation (Relation relationId) = do
    cache  <- gets ctxCache
    cached <- relationLiftIO $ H.lookup cache relationId
    case cached of
        Just value -> return $ fromDyn (fst value) (error "Type mismatch for the cached relational data")
        Nothing -> do
            -- Otherwise, we read from the store
            store <- gets ctxStore
            (result, newStore) <- relationLiftIO $ runStateT (storeRead relationId) store
            puts ctxStore newStore
            -- And update the cache
            relationLiftIO $ H.insert cache relationId (toDyn result, storeUpdate relationId result)
            -- Yield result
            return result
    
updateRelation :: (Store s, Related a) => Relation a -> a -> Relational s ()
updateRelation (Relation relationId) value = do
    -- Update the cache
    cache <- gets ctxCache
    relationLiftIO $ H.insert cache relationId (toDyn value, storeUpdate relationId value)

deleteRelation :: (Store s, Related a) => Relation a -> Relational s ()
deleteRelation = undefined -- TODO

relationLiftIO :: IO a -> Relational s a
relationLiftIO = Relational . liftIO