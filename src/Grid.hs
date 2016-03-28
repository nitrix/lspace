module Grid
    ( Grid
    , empty
    , insert
    --, adjust
    , lookup
    , fromList
    -- , range
    )
where

import Prelude hiding (lookup)
import Control.Lens
import Coordinate
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Object

data Grid k v = MkGrid (M.Map k (M.Map k (S.Set v))) (M.Map v (S.Set (k, k)))

empty :: Grid k v
empty = MkGrid M.empty M.empty

insert :: (Ord k, Ord v) => k -> k -> v -> Grid k v -> Grid k v
insert x y v (MkGrid left right) = MkGrid newLeft newRight
    where
        newLeft  = M.insertWith (M.unionWith S.union) x (M.singleton y (S.singleton v)) left
        newRight = M.insertWith (S.union) v (S.singleton (x, y)) right

lookup :: Ord k => k -> k -> Grid k v -> S.Set v
lookup x y (MkGrid left right) = fromMaybe S.empty $ M.lookup y =<< M.lookup x left

fromList :: (Ord k, Ord v) => [(k, k, v)] -> Grid k v
fromList xs = foldl' (\m (x, y, v) -> insert x y v m) empty xs

adjust :: Ord k => (v -> v) -> k -> k -> Grid k v -> Grid k v
adjust f x y (MkGrid left right) = MkGrid newLeft newRight
    where