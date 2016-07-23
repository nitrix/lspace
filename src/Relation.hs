{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Relation where

import qualified Data.Aeson as J
import Data.IORef
import Data.Sequence as S
import qualified Data.ByteString.Lazy as LB
import System.Directory
import System.Mem.Weak

import Types.Cache
import Types.Relation

-- trace :: b -> a -> a
-- trace = const id

-- New %~ lens combinator that's strict (thanks to puregreen)
data Id a = Id {runId :: !a} deriving Functor
(%~!) :: ((a -> Id b) -> c -> Id d) -> (a -> b) -> c -> d
l %~! f = runId . l (Id . f)

readRelation :: forall a. J.FromJSON a => IORef Cache -> Relation a -> IO (Maybe a)
readRelation refCache (MkRelation relation) = do
    -- Read the link unsafe bastraction
    (i, r) <- readIORef relation
    case r of
        -- Determines if we have a link reference
        Nothing -> do
            result <- loadFreshRelationId i
            case result of
                Nothing -> return Nothing
                Just a  -> do
                    -- writeIORef link a
                    modifyIORef' relation $ const a
                    readRelation refCache $ MkRelation relation
        Just x  -> do
            final <- deRefWeak x
            -- Does the reference point to something that still exists
            case final of
                Just z  -> do
                    v <- readIORef z
                    return $ Just v
                Nothing -> do
                    result <- loadFreshRelationId i
                    case result of
                        Nothing -> return Nothing
                        Just a  -> do
                            -- writeIORef link a
                            modifyIORef' relation $ const a
                            readRelation refCache $ MkRelation relation
    where
        loadFreshRelationId :: Int -> IO (Maybe (Int, Maybe (Weak (IORef a))))
        loadFreshRelationId i = do
            ok <- doesFileExist filepath
            if ok
            then do
                json <- LB.readFile filepath
                case J.decode json of
                    Nothing -> do
                        return Nothing
                    Just d  -> do
                        ref <- newIORef d

                        modifyIORef' refCache $ cacheRelations %~! \relations -> (
                                                if S.length relations >= maxRelationsCache
                                                then S.drop 1 relations
                                                else relations
                                              ) |> MkAnyIORef ref

                        weakRef <- mkWeakIORef ref (return ())
                        return . Just $ (i, Just weakRef)
            else do
                return Nothing
            where
                filepath = "data/demo/" ++ show i ++ ".json" -- TODO: This has to be fixed
                maxRelationsCache = 1000
