module Coordinate where

import Prelude hiding (Left, Right)
import Linear (V2(V2))
import Linear.Affine (Point(P))
import Control.Lens

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection

newtype Coordinate = MkCoordinate { _coordinate :: Point V2 Integer } deriving (Eq, Ord, Show)

-- TODO: Wait for GHC8 and then switch to makeLenses
coordinateX :: Lens' Coordinate Integer
coordinateX f s = let (P (V2 cx cy)) = _coordinate s in
    (\x -> coordinate x cy) <$> f cx

-- TODO: Wait for GHC8 and then switch to makeLenses
coordinateY :: Lens' Coordinate Integer
coordinateY f s = let (P (V2 cx cy)) = _coordinate s in
    (\y -> coordinate cx y) <$> f cy

coordinate :: Integer -> Integer -> Coordinate
coordinate x y = MkCoordinate $ P $ V2 x y

coordinateMove :: Direction -> Coordinate -> Coordinate
coordinateMove UpDirection    = coordinateY %~ subtract 1
coordinateMove DownDirection  = coordinateY %~ (+1)
coordinateMove LeftDirection  = coordinateX %~ subtract 1
coordinateMove RightDirection = coordinateX %~ (+1)

defaultCoordinate :: Coordinate
defaultCoordinate = coordinate 0 0
