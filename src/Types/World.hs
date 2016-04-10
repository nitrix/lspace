module Types.World
    ( World(..)
    , WorldShips
    , defaultWorld
    , worldShips
    -- , worldNextObjectId
) where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Ship as H
import Types.Coordinate
import Types.Object

type WorldShips = S.Set H.Ship

data World = MkWorld
    { _worldShips :: WorldShips
    }

-- Lenses
worldShips :: Lens' World WorldShips
worldShips = lens _worldShips (\s x -> s { _worldShips = x })

-- Empty world
defaultWorld :: World
defaultWorld = MkWorld
    { _worldShips = S.empty
    }
