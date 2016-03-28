module Grid
    ( Grid
    , empty
    , insert
    --, adjust
    --, lookup
    --, fromList
    -- , range
    )
where

import Control.Lens
import Coordinate
import qualified Data.Map as M
import qualified Data.Set as S
import Object

data Grid k v = MkGrid (M.Map k (M.Map k (S.Set v))) (M.Map v (S.Set (k, k)))

empty :: Grid k v
empty = MkGrid M.empty M.empty

insert :: (Ord k, Ord v) => k -> k -> v -> Grid k v -> Grid k v
insert x y v (MkGrid left right) = MkGrid newLeft newRight
    where
        newLeft   = M.insertWith (M.unionWith S.union) x (M.singleton y (S.singleton v)) left
        newRight  = M.empty -- TODO