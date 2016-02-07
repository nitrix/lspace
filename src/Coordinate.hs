module Coordinate where

import Linear (V2(V2))
import Linear.Affine (Point(P))
import Control.Lens

newtype Coordinate = MkCoordinate { getCoordinate :: Point V2 Integer }

coordinateX :: Lens' Coordinate Integer
coordinateX f s = let (P (V2 cx cy)) = getCoordinate s in
    (\x -> MkCoordinate $ P $ V2 x cy) <$> f cx

coordinateY :: Lens' Coordinate Integer
coordinateY f s = let (P (V2 cx cy)) = getCoordinate s in
    (\y -> MkCoordinate $ P $ V2 cx y) <$> f cy

defaultCoordinate :: Coordinate
defaultCoordinate = MkCoordinate $ P $ V2 0 0
