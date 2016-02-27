module Coordinate where

import Prelude hiding (Left, Right)
import Linear (V2(V2))
import Linear.Affine (Point(P))
import Control.Lens

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection

newtype Coordinate = MkCoordinate { _coordinate :: Point V2 Integer } deriving (Eq, Ord, Show)

-- Lenses
coordinateX :: Lens' Coordinate Integer
coordinateY :: Lens' Coordinate Integer
coordinateX f s = let (P (V2 cx cy)) = _coordinate s in (\x -> coordinate x cy) <$> f cx
coordinateY f s = let (P (V2 cx cy)) = _coordinate s in (\y -> coordinate cx y) <$> f cy

-- | Simplified Coordinate constructor
coordinate :: Integer -> Integer -> Coordinate
coordinate x y = MkCoordinate $ P $ V2 x y

-- | Compute a new coordinate relative to an existing coordinate in a given direction
coordinateMove :: Direction -> Coordinate -> Coordinate
coordinateMove UpDirection    = coordinateY %~ subtract 1
coordinateMove DownDirection  = coordinateY %~ (+1)
coordinateMove LeftDirection  = coordinateX %~ subtract 1
coordinateMove RightDirection = coordinateX %~ (+1)

-- | Center point
defaultCoordinate :: Coordinate
defaultCoordinate = coordinate 0 0
