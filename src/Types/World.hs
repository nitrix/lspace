module Types.World
    ( World(..)
    , WorldShips
    , defaultWorld
    , worldShips
) where

import Control.Lens
import qualified Types.Ship as H

type WorldShips = [H.Ship]

data World = MkWorld
    { _worldShips :: WorldShips
    }

-- Lenses
worldShips :: Lens' World WorldShips
worldShips = lens _worldShips (\s x -> s { _worldShips = x })

defaultWorld :: World
defaultWorld = MkWorld
    { _worldShips = []
    }
