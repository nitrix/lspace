module Coordinate
    ( Coordinate(getCoordinate) -- TODO: ewww
    , Direction(..)
    , coordinate
    , coordinateMove
    , coordinateX
    , coordinateY
    , defaultCoordinate
    ) where

import Control.Lens
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Prelude hiding (Left, Right)

data Direction = UpDirection
               | RightDirection
               | DownDirection
               | LeftDirection
               deriving (Show, Bounded, Enum)

newtype Coordinate = Coordinate { getCoordinate :: Point V2 Integer } deriving (Eq)

instance Ord Coordinate where
    c1 > c2  = let ((P(V2 x1 y1)), (P(V2 x2 y2))) = (getCoordinate c1, getCoordinate c2) in x1 > x2 && y1 > y2
    c1 >= c2 = let ((P(V2 x1 y1)), (P(V2 x2 y2))) = (getCoordinate c1, getCoordinate c2) in x1 >= x2 && y1 >= y2
    c1 <= c2 = let ((P(V2 x1 y1)), (P(V2 x2 y2))) = (getCoordinate c1, getCoordinate c2) in x1 <= x2 && y1 <= y2

-- Lenses
coordinateX :: Lens' Coordinate Integer
coordinateY :: Lens' Coordinate Integer
coordinateX = lens (view _x . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _x .~ z)
coordinateY = lens (view _y . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _y .~ z)

-- | Simplified Coordinate constructor
coordinate :: Integer -> Integer -> Coordinate
coordinate x y = Coordinate $ P $ V2 x y

-- | Compute a new coordinate relative to an existing coordinate in a given direction
coordinateMove :: Direction -> Coordinate -> Coordinate
coordinateMove UpDirection    = coordinateY %~ subtract 1
coordinateMove DownDirection  = coordinateY %~ (+1)
coordinateMove LeftDirection  = coordinateX %~ subtract 1
coordinateMove RightDirection = coordinateX %~ (+1)

-- | Center point
defaultCoordinate :: Coordinate
defaultCoordinate = coordinate 0 0
