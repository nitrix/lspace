module Object where

import Coordinate
import Tile

data Object = MkObject { _objectCoordinate :: Coordinate
                       , _objectTile :: Tile
                       }