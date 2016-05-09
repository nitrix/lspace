module Types.World
    ( World(..)
    , WorldShips
    , WorldObjects
    , defaultWorld
    , worldObjects
    , worldShips
) where

import Control.Lens
import qualified Data.Map as M
import qualified Ship as H
import Types.Coordinate
import Types.Object

type WorldShips   = [(Coordinate, H.Ship)]
type WorldObjects = M.Map ObjectId Object

data World = MkWorld
    { _worldShips   :: WorldShips
    , _worldObjects :: WorldObjects
    }

-- Lenses
worldObjects :: Lens' World WorldObjects
worldShips   :: Lens' World WorldShips
worldObjects = lens _worldObjects (\s x -> s { _worldObjects = x })
worldShips   = lens _worldShips (\s x -> s { _worldShips = x })

defaultWorld :: World
defaultWorld = MkWorld
    { _worldShips   = []
    , _worldObjects = M.empty
    }
