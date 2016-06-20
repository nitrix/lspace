module Object.Player where

import Control.Monad.State

import Types.Coordinate
import Types.Message
import Types.Object
import Types.Sprite

data Player = MkPlayer
    { _playerHealth :: Int
    }

-- This only initializes the original object; it is NOT a permanent binding that everytime
-- objSprite is called playerSprite would be called. They act independently.
playerObject :: Object -> Player -> Object
playerObject obj p = obj
    { objSolid  = False
    , objSprite = playerSprite obj
    , objMsg    = fantasticObjMsg playerMsg playerObject p
    }

defaultPlayer :: Player
defaultPlayer = MkPlayer 
    { _playerHealth = 100
    }

playerSprite :: Object -> Sprite
playerSprite o = case objFacing o of
    North -> sprite 1 0 ZOnTop
    South -> sprite 1 2 ZOnTop
    West  -> sprite 1 1 ZOnTop
    East  -> sprite 1 3 ZOnTop

playerMsg :: Message -> State Player [Message]
playerMsg m = do
    case m of
        _ -> return []
