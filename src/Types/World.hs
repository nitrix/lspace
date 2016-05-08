module Types.World
    ( World(..)
    , WorldShips
    , defaultWorld
    , worldShips
) where

import Control.Lens
import qualified Data.Map as M
import qualified Ship as H
import Types.Coordinate

type WorldShips = M.Map Coordinate H.Ship

data World = MkWorld
    { _worldShips :: WorldShips
    }

-- Lenses
worldShips :: Lens' World WorldShips
worldShips = lens _worldShips (\s x -> s { _worldShips = x })

-- Empty world
defaultWorld :: World
defaultWorld = MkWorld
    { _worldShips = M.empty
    }
