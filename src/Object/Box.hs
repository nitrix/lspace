-- module Object.Box (boxObject, defaultBox, Box(..), BoxState(..)) where
module Object.Box where

import Message
import Object

data BoxState = BoxOpened | BoxClosed | BoxClosedLocked
data Box = MkBox { boxState :: BoxState }

boxObject :: Object -> Box -> Object
boxObject o x = o
    { objSprite = boxSprite x
    , objUpdate = boxObject o . boxUpdate x
    , objMsg = boxMsg x
    }

defaultBox :: Box
defaultBox = MkBox 
    { boxState = BoxOpened
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
boxUpdate x m = case m of
    InteractMsg -> case boxState x of
        BoxOpened       -> x { boxState = BoxClosed }
        BoxClosed       -> x { boxState = BoxOpened }
        BoxClosedLocked -> x
    _ -> x

boxMsg :: Box -> Message -> [Message]
boxMsg _ _ = []
