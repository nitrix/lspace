{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Grid
    ( Grid
    , Gridable(..)
    , empty
    , insert
    , adjustR
    , lookup
    , lookupR
    , fromList
    , toList
    -- , range
    )
where

import Prelude hiding (lookup)
import Control.Lens
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

-- class Gridable c k where
--     fromGrid :: (k, k) -> c
--     toGrid :: c -> (k, k)

class Ord k => Gridable c k | c -> k where
    fromGrid :: (k, k) -> c
    toGrid   :: c -> (k, k)

-- One grid cell can have multiple object ids, but one object id is always at only one coordinate
data Gridable c k => Grid c k v = MkGrid (M.Map k (M.Map k (S.Set v))) (M.Map v (S.Set c))

empty :: Gridable c k => Grid c k v
empty = MkGrid M.empty M.empty

insert :: (Gridable c k, Ord c, Ord k, Ord v) => c -> v -> Grid c k v -> Grid c k v
insert c v (MkGrid left right) = MkGrid newLeft newRight
    where
        (x, y)   = toGrid c
        newLeft  = M.insertWith' (M.unionWith S.union) x (M.singleton y (S.singleton v)) left
        newRight = M.insertWith' (S.union) v (S.singleton c) right

lookup :: (Gridable c k, Ord k) => c -> Grid c k v -> S.Set v
lookup c (MkGrid left _) = fromMaybe S.empty $ M.lookup y =<< M.lookup x left
    where
        (x, y) = toGrid c

lookupR :: (Gridable c k, Ord v, Ord k) => v -> Grid c k v -> S.Set c
lookupR v (MkGrid _ right) = fromMaybe S.empty $ M.lookup v right

fromList :: (Gridable c k, Ord c, Ord v) => [(c, v)] -> Grid c k v
fromList xs = foldl' (\m (k, v) -> insert k v m) empty xs

-- Update the coordinates of an object. Could be more efficient instead of doing deletions and insertions.
adjustR :: (Gridable c k, Ord c, Ord v) => v -> c -> Grid c k v -> Grid c k v
adjustR v c g@(MkGrid left right) = insert c v $ MkGrid cleanLeft cleanRight
    where
        oldKeys    = lookupR v g
        cleanLeft  = foldl' (\m c -> let (ox, oy) = toGrid c in M.adjust (M.adjust (S.delete v) oy) ox m) left oldKeys
        cleanRight = M.delete v right

toList :: (Gridable c k) => Grid c k v -> [(k, k, v)]
toList (MkGrid left _) = concat . concat $
    map (\(k1,m) -> map (\(k2, s) -> map (\v -> (k1, k2, v)) (S.toList s)) (M.toList m)) (M.toList left)
