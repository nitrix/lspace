module Object.Box
    ( boxObject
    , defaultBox
    , Box(..)
    , BoxState(..)
    )
where

import Control.Monad.State

import Types.Message
import Types.Object
import Types.Sprite

data BoxState = BoxOpened | BoxClosed

data Box = MkBox
    { _boxState  :: BoxState
    }

boxObject :: Object -> Box -> Object
boxObject obj box = obj
    { objSolid  = True
    , objSprite = boxSprite box
    , objMsg    = \msg -> boxObject obj <$> runState (boxMsg msg) box
    }

defaultBox :: Box
defaultBox = MkBox 
    { _boxState = BoxOpened
    }

boxSprite :: Box -> Sprite
boxSprite box = case _boxState box of
    BoxOpened -> sprite 0 2 ZOnGround
    BoxClosed -> sprite 0 1 ZOnGround

boxMsg :: Message -> State Box [Message]
boxMsg _ = return []
