module Coordinate where

import Prelude hiding (Left, Right)
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Control.Lens

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection

type Coordinate = Point V2 Integer

-- Lenses
coordinateX :: Lens' Coordinate Integer
coordinateY :: Lens' Coordinate Integer
coordinateX = _x
coordinateY = _y

-- | Simplified Coordinate constructor
coordinate :: Integer -> Integer -> Coordinate
coordinate x y = P $ V2 x y

-- | Compute a new coordinate relative to an existing coordinate in a given direction
coordinateMove :: Direction -> Coordinate -> Coordinate
coordinateMove UpDirection    = coordinateY %~ subtract 1
coordinateMove DownDirection  = coordinateY %~ (+1)
coordinateMove LeftDirection  = coordinateX %~ subtract 1
coordinateMove RightDirection = coordinateX %~ (+1)

-- | Center point
defaultCoordinate :: Coordinate
defaultCoordinate = coordinate 0 0
