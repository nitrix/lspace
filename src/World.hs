module World
    ( World
    , demoContent
    , defaultWorld
    , worldObjectsAt
    , worldMoveObject
) where

import qualified Assoc as A
import Coordinate
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Object
import Object.Box
import Object.Player
import System.Message

type Layer = A.Assoc Coordinate ObjectId
type ObjectMapping = M.Map ObjectId Object

data World = MkWorld
    { _layer   :: Layer -- TODO: multiple layers
    , _objects :: ObjectMapping
    }

layer :: Lens' World Layer
layer f s = (\x -> s { _layer = x }) <$> f (_layer s)

objects :: Lens' World ObjectMapping
objects f s = (\x -> s { _objects = x }) <$> f (_objects s)

defaultWorld :: World
defaultWorld = MkWorld
    { _layer   = demoContent
    , _objects = demoObjects
    }

demoObjects :: M.Map ObjectId Object
demoObjects = M.fromList
    [ (0, boxObject defaultObject defaultBox)
    , (1, boxObject defaultObject $ defaultBox { _boxLocked = True })
    , (2, playerObject defaultObject defaultPlayer)
    ]

demoContent :: A.Assoc Coordinate ObjectId
demoContent = A.fromList
    [ (coordinate 0 0, 0)
    , (coordinate 1 0, 0)
    , (coordinate 0 1, 0)
    , (coordinate 2 1, 1)
    , (coordinate 1 2, 0)
    , (coordinate 3 1, 0)
    , (coordinate 0 0, 1)
    , (coordinate 1 3, 1)
    , (coordinate 5 2, 0)
    , (coordinate 5 5, 0)
    , (coordinate 5 6, 2)
    ]

worldObjectById :: World -> ObjectId -> Maybe Object
worldObjectById w oid = M.lookup oid $ view objects w

worldObjectIdsAt :: World -> Coordinate -> [ObjectId]
worldObjectIdsAt w c = S.toList $ A.lookup c $ view layer w

worldObjectsAt :: World -> Coordinate -> [Object]
worldObjectsAt w c = mapMaybe (worldObjectById w) (worldObjectIdsAt w c)

-- TODO: Needs to handle messag responses eventually
worldMessage :: Message -> ObjectId -> World -> World
worldMessage msg objid = objects %~ M.adjust newObject objid
    where
        newObject o = snd $ msgedObject o
        msgedObject o = objMsg o msg

-- TODO: Needs a serious rewrite eventually
worldMoveObject :: Direction -> ObjectId -> World -> World
worldMoveObject direction objid w =
    if (all (==False) $ map objSolid $ worldObjectsAt w newCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z = worldMessage (MkMessage $ MovedMsg direction) objid z
        updateCoordinate = layer %~ A.adjustR (coordinateMove direction) objid
        currentCoordinate = S.elemAt 0 $ A.lookupR objid (w ^. layer)
        newCoordinate = coordinateMove direction currentCoordinate
