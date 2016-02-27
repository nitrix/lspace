module Object.Player where

import Control.Lens
import Control.Monad.State
import Coordinate
import Object
import Sprite
import System.Message

data Player = MkPlayer
    { _playerHealth :: Int
    , _playerDirection :: Direction
    }

playerDirection :: Lens' Player Direction
playerDirection f p = (\s -> p { _playerDirection = s } ) <$> (f $ _playerDirection p)

playerObject :: Object -> Player -> Object
playerObject obj p = obj
    { objSolid = False
    , objSprite = playerSprite p
    , objMsg = \msg -> playerObject obj <$> runState (playerMsg msg) p
    }

defaultPlayer :: Player
defaultPlayer = MkPlayer 
    { _playerHealth = 100
    , _playerDirection  = DownDirection
    }

playerSprite :: Player -> Sprite
playerSprite p = case _playerDirection p of
    UpDirection    -> sprite 1 0
    DownDirection  -> sprite 1 2
    LeftDirection  -> sprite 1 1
    RightDirection -> sprite 1 3

playerMsg :: Message -> State Player [Message]
playerMsg m = do
    case msgType m of
        (MovedMsg direction) -> modify $ playerDirection .~ direction
        _ -> return ()
    return []
