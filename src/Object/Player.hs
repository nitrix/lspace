module Object.Player where

import Control.Monad.State
import Message
import Object
import Sprite

data Direction = FacingUp | FacingDown | FacingLeft | FacingRight

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
    , playerDirection  = FacingUp
    }

playerSprite :: Player -> Sprite
playerSprite p = case playerDirection p of
    FacingUp -> sprite 1 0
    FacingDown -> sprite 1 2
    FacingLeft -> sprite 1 1
    FacingRight -> sprite 1 3

playerMsg :: Message -> State Player [Message]
playerMsg m = do
    case m of
        _ -> return []
