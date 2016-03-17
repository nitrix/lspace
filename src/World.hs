module World
    ( World(..)
    , WorldLayer
    , WorldObjects
    , defaultWorld
    , worldLayer
    , worldObjects
) where

import qualified Assoc as A
import Coordinate
import Control.Lens
import qualified Data.Map as M
import Object

type WorldLayer = A.Assoc Coordinate ObjectId
type WorldObjects = M.Map ObjectId Object

data World = MkWorld
    { _worldLayer   :: WorldLayer -- TODO: multiple layers
    , _worldObjects :: WorldObjects
    }

-- Lenses
worldLayer   :: Lens' World WorldLayer
worldObjects :: Lens' World WorldObjects
worldLayer   = lens _worldLayer   (\s x -> s { _worldLayer = x })
worldObjects = lens _worldObjects (\s x -> s { _worldObjects = x })

-- Empty world
defaultWorld :: World
defaultWorld = MkWorld
    { _worldLayer   = A.empty
    , _worldObjects = M.empty
    }
