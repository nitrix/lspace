-- module Object.Box (boxObject, defaultBox, Box(..), BoxState(..)) where
module Object.Box where

import Control.Monad.State
import Message
import Object
import Sprite

data BoxState = BoxOpened | BoxClosed | BoxClosedLocked
data Box = MkBox { boxState :: BoxState }

boxObject :: Object -> Box -> Object
boxObject obj box = obj
    { objSolid = True
    , objSprite = boxSprite box
    , objMsg = \msg -> boxObject obj <$> runState (boxMsg msg) box
    }

defaultBox :: Box
defaultBox = MkBox 
    { boxState = BoxOpened
    }

boxSprite :: Box -> Sprite
boxSprite box = case boxState box of
    BoxOpened       -> opened
    BoxClosed       -> closed
    BoxClosedLocked -> closed
    where
        opened = [spritePart 0 0 2 2, spritePart 0 1 0 0]
        closed = sprite 3 3

boxMsg :: Message -> State Box [Message]
boxMsg m = do
    case m of
        InteractMsg -> boxInteract
        _ -> return []

boxInteract :: State Box [Message]
boxInteract = do
    box <- get
    case boxState box of
        BoxOpened       -> (modify $ \x -> x { boxState = BoxClosed }) >> return []
        BoxClosed       -> (modify $ \x -> x { boxState = BoxOpened }) >> return []
        BoxClosedLocked -> return []
