module Types.World
    ( World(..)
    , WorldShips
    , WorldObjects
    , defaultWorld
    , worldNextObjId
    , worldNextShipId
    , worldObjects
    , worldShips
) where

import Control.Lens
import qualified Data.Map as M

import Types.Id
import Types.Object
import qualified Types.Ship as H

type WorldShips     = M.Map ShipId H.Ship
type WorldObjects   = M.Map ObjectId Object

data World = MkWorld
    { _worldNextObjId  :: ObjectId
    , _worldNextShipId :: ShipId
    , _worldObjects    :: WorldObjects
    , _worldShips      :: WorldShips
    }

-- Lenses
worldNextObjId  :: Lens' World ObjectId
worldNextShipId :: Lens' World ShipId
worldObjects    :: Lens' World WorldObjects
worldShips      :: Lens' World WorldShips
worldNextObjId  = lens _worldNextObjId  (\s x -> s { _worldNextObjId  = x })
worldNextShipId = lens _worldNextShipId (\s x -> s { _worldNextShipId = x })
worldObjects    = lens _worldObjects    (\s x -> s { _worldObjects    = x })
worldShips      = lens _worldShips      (\s x -> s { _worldShips      = x })

defaultWorld :: World
defaultWorld = MkWorld
    { _worldNextObjId  = 0
    , _worldNextShipId = 0
    , _worldShips      = M.empty
    , _worldObjects    = M.empty
    }
