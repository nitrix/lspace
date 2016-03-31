module Object.Player where

import Control.Lens
import Control.Monad.State
import Coordinate
import Message
import Object
import Sprite

data Player = MkPlayer
    { _playerHealth    :: Int
    , _playerDirection :: Direction
    }

-- Lenses
playerDirection :: Lens' Player Direction
playerDirection = lens _playerDirection (\s x -> s { _playerDirection = x })

playerObject :: Object -> Player -> Object
playerObject obj p = obj
    { objSolid  = False
    , objSprite = playerSprite p
    , objFacing = view playerDirection p
    , objMsg    = \msg -> playerObject obj <$> runState (playerMsg msg) p
    }

defaultPlayer :: Player
defaultPlayer = MkPlayer 
    { _playerHealth     = 100
    , _playerDirection  = South
    }

playerSprite :: Player -> Sprite
playerSprite p = case _playerDirection p of
    North -> sprite 1 0 ZOnTop
    South -> sprite 1 2 ZOnTop
    West  -> sprite 1 1 ZOnTop
    East  -> sprite 1 3 ZOnTop

playerMsg :: Message -> State Player [Message]
playerMsg m = do
    case m of
        MovedMsg direction -> [] <$ (modify $ playerDirection .~ direction)
        RotateMsg          -> [] <$ (modify $ playerDirection %~ (\d -> (enumFrom d ++ [minBound..maxBound]) !! 1))
        _                  -> return []
