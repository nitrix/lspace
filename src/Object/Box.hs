module Object.Box (boxObject, defaultBox, Box(..), BoxState(..)) where

import Message
import Object

data BoxState = BoxOpened | BoxClosed | BoxClosedLocked

data Box = MkBox
    { boxState :: BoxState
    , boxPin   :: Int
    }

boxObject :: Box -> Object
boxObject x = defaultObject
    { objSprite = boxSprite x
    , objUpdate = boxObject . boxUpdate x
    }

defaultBox :: Box
defaultBox = MkBox 
    { boxState = BoxOpened
    , boxPin = 0
    }

boxSprite :: Box -> (Int, Int)
boxSprite x = case boxState x of
    BoxOpened       -> opened
    BoxClosed       -> closed
    BoxClosedLocked -> closed
    where
        opened = (2, 2)
        closed = (3, 3)

boxUpdate :: Box -> Message -> Box
boxUpdate x msg = case msg of
    InteractMsg -> case boxState x of
        BoxOpened       -> x { boxState = BoxClosed }
        BoxClosed       -> x { boxState = BoxOpened }
        BoxClosedLocked -> x
    _ -> x
