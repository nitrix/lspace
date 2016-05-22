{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where

import Prelude hiding (lookup)
import Data.Foldable (foldl')
import qualified Data.Set as S

class Gridable c k | c -> k where
    extractX :: (Eq c, Ord k) => c -> k
    extractY :: (Eq c, Ord k) => c -> k

data GridAxis = AxisX | AxisY deriving (Eq, Ord, Show)

-- | Grid c v is a spatial partitioning data structure that maps a coordinate type @c to a set of values of type @v.
data Grid c v = MkGrid
    { gLeft   :: Grid c v
    , gCenter :: (c, S.Set v)
    , gRight  :: Grid c v
    , gAxis   :: GridAxis
    } | GridEmpty GridAxis
    deriving (Eq, Ord, Show)

empty :: Grid c v
empty = GridEmpty AxisX

fromList :: (Gridable c k, Eq c, Ord k, Ord v) => [(c, v)] -> Grid c v
fromList xs = foldl' (flip $ uncurry insert) empty xs

-- Tested
insert :: (Gridable c k, Eq c, Ord k, Ord v) => c -> v -> Grid c v -> Grid c v
insert coord value (GridEmpty axis) = MkGrid
    { gLeft   = GridEmpty $ oppAxis axis
    , gCenter = (coord, S.singleton value)
    , gRight  = GridEmpty $ oppAxis axis
    , gAxis   = axis
    }
    where
        oppAxis a = if a == AxisX then AxisY else AxisX
insert coord value grid = case (extractor coord) `compare` (extractor pivot) of
    LT -> grid { gLeft = insert coord value $ gLeft grid }
    EQ -> if coord == pivot
          then grid { gCenter = S.insert value <$> gCenter grid }
          else grid { gLeft = insert coord value $ gLeft grid }
    GT -> grid { gRight = insert coord value $ gRight grid }
    where
        pivot     = fst . gCenter $ grid
        extractor = if gAxis grid == AxisX then extractX else extractY

-- Tested
lookup :: (Gridable c k, Eq c, Ord k) => c -> Grid c v -> S.Set v
lookup _ (GridEmpty _) = S.empty
lookup coord grid = case (extractor coord) `compare` (extractor pivot) of
    LT -> lookup coord $ gLeft grid
    EQ -> if coord == pivot
          then snd . gCenter $ grid
          else lookup coord $ gLeft grid
    GT -> lookup coord $ gRight grid
    where
        pivot = fst . gCenter $ grid
        extractor = if gAxis grid == AxisX then extractX else extractY

remove :: c -> v -> Grid c v -> Grid c v
remove coord value grid = grid -- TODO

range :: c -> c -> Grid c v -> S.Set v 
range low high grid = S.empty -- TODO: set unions
