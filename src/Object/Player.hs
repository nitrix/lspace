module Object.Player where

import Control.Monad.State
import Coordinate
import Message
import Object
import Sprite

data Player = MkPlayer
    { playerHealth :: Int
    , playerDirection :: Direction
    }

playerObject :: Object -> Player -> Object
playerObject obj p = obj
    { objSolid = False
    , objSprite = playerSprite p
    , objMsg = \msg -> playerObject obj <$> runState (playerMsg msg) p
    }

defaultPlayer :: Player
defaultPlayer = MkPlayer 
    { playerHealth = 100
    , playerDirection  = DownDirection
    }

playerSprite :: Player -> Sprite
playerSprite p = case playerDirection p of
    UpDirection    -> sprite 1 0
    DownDirection  -> sprite 1 2
    LeftDirection  -> sprite 1 1
    RightDirection -> sprite 1 3

playerMsg :: Message -> State Player [Message]
playerMsg m = do
    case m of
        _ -> return []
