{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where

import Prelude hiding (lookup)
import Data.Foldable (foldl')
import qualified Data.Set as S

class Gridable c k | c -> k where
    extractX :: Ord k => c -> k
    extractY :: Ord k => c -> k

data GridAxis = AxisX | AxisY deriving (Eq, Ord, Show)

-- | Grid c v is a spatial partitioning data structure that maps a coordinate type @c to a set of values of type @v.
data Grid c k v = MkGrid
    { gLeft   :: Grid c k v
    -- , gPivot :: k
    , gCenter :: (c, S.Set v)
    -- , gCenter :: Map c (S.Set v)
    , gRight  :: Grid c k v
    , gAxis   :: GridAxis
    }
    | MkGridCell c v
    | MkGridEmpty
    deriving (Eq, Ord, Show)

empty :: Grid c k v
empty = MkGridEmpty AxisX

fromList :: (Gridable c k, Ord k, Ord v) => [(c, v)] -> Grid c k v
fromList xs = foldl' (\g x -> uncurry insert x g) empty xs

insert :: (Gridable c k, Ord k, Ord v) => c -> v -> Grid c k v -> Grid c k v
insert coord value (GridEmpty axis) = MkGrid
    { gLeft   = GridEmpty $ oppAxis axis
    , gCenter = (coord, S.singleton value)
    , gRight  = GridEmpty $ oppAxis axis
    , gAxis   = axis
    }
    where
        oppAxis a = if a == AxisX then AxisY else AxisX
insert coord value grid = case (separator coord) `compare` (separator pivot) of
    LT -> grid { gLeft   = insert coord value $ gLeft grid }
    EQ -> grid { gCenter = S.insert value <$> gCenter grid }
    GT -> grid { gRight  = insert coord value $ gRight grid }
    where
        pivot     = fst . gCenter $ grid
        separator = if gAxis grid == AxisX then extractX else extractY

-- Tested
lookup :: (Gridable c k, Ord k) => c -> Grid c k v -> S.Set v
lookup _ (GridEmpty _) = S.empty
lookup coord grid = case (separator coord) `compare` (separator pivot) of
    LT -> lookup coord  $ gLeft grid
    EQ -> snd . gCenter $ grid
    GT -> lookup coord  $ gRight grid
    where
        pivot = fst . gCenter $ grid
        separator = if gAxis grid == AxisX then extractX else extractY

remove :: c -> v -> Grid c k v -> Grid c k v
remove coord value grid = grid -- TODO

range :: c -> c -> Grid c k v -> S.Set v 
range low high grid = S.empty -- set unions
