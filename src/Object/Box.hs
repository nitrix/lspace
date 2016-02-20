-- module Object.Box (boxObject, defaultBox, Box(..), BoxState(..)) where
module Object.Box where

import Control.Monad.State
import Message
import Object

data BoxState = BoxOpened | BoxClosed | BoxClosedLocked
data Box = MkBox { boxState :: BoxState }

boxObject :: Object -> Box -> Object
boxObject o x = o
    { objSprite = boxSprite x
    , objUpdate = \m -> boxObject o <$> runState (boxUpdate m) x
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

boxUpdate :: Message -> State Box [Message]
boxUpdate m = do
    x <- get
    case m of
        InteractMsg -> case boxState x of
            BoxOpened       -> (modify $ \x -> x { boxState = BoxClosed }) >> return []
            BoxClosed       -> (modify $ \x -> x { boxState = BoxOpened }) >> return []
            BoxClosedLocked -> return []
        _ -> return []
