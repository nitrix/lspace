module Types.World
    ( World(..)
    , WorldLayer
    , WorldObjects
    , defaultWorld
    , worldLayer
    , worldObjects
    -- , worldNextObjectId
) where

import qualified Grid as A
import Control.Lens
import qualified Data.Map as M
import Types.Coordinate
import Types.Object

type WorldLayer = A.Grid Coordinate Integer ObjectId
type WorldObjects = M.Map ObjectId Object

data World = MkWorld
    { _worldLayer        :: WorldLayer -- TODO: multiple layers
    , _worldObjects      :: WorldObjects
    -- , _worldNextObjectId :: ObjectId
    }

-- Lenses
worldLayer        :: Lens' World WorldLayer
worldObjects      :: Lens' World WorldObjects
-- worldNextObjectId :: Lens' World ObjectId
worldLayer        = lens _worldLayer        (\s x -> s { _worldLayer        = x })
worldObjects      = lens _worldObjects      (\s x -> s { _worldObjects      = x })
-- worldNextObjectId = lens _worldNextObjectId (\s x -> s { _worldNextObjectId = x })

-- Empty world
defaultWorld :: World
defaultWorld = MkWorld
    { _worldLayer        = A.empty
    , _worldObjects      = M.empty
    -- , _worldNextObjectId = 0
    }
