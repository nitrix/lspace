module Object.Box
    ( boxObject
    , defaultBox
    , Box(..)
    , BoxState(..)
    )
where

import Control.Monad.State
import Message
import Object
import Sprite

data BoxState = BoxOpened | BoxClosed

data Box = MkBox
    { boxState  :: BoxState
    , boxLocked :: Bool
    }

boxObject :: Object -> Box -> Object
boxObject obj box = obj
    { objSolid  = True
    , objSprite = boxSprite box
    , objMsg    = \msg -> boxObject obj <$> runState (boxMsg msg) box
    }

defaultBox :: Box
defaultBox = MkBox 
    { boxState  = BoxOpened
    , boxLocked = False
    }

boxSprite :: Box -> Sprite
boxSprite box = case boxState box of
    BoxOpened -> sprite 0 2
    BoxClosed -> sprite 0 1

boxMsg :: Message -> State Box [Message]
boxMsg m = do
    case m of
        InteractMsg -> boxInteract
        _           -> return []

boxInteract :: State Box [Message]
boxInteract = do
    box <- get
    if (boxLocked box) then
        return []
    else
        case boxState box of
            BoxOpened -> (modify $ \x -> x { boxState = BoxClosed }) >> return []
            BoxClosed -> (modify $ \x -> x { boxState = BoxOpened }) >> return []
