module Object.Box
    ( boxObject
    , defaultBox
    , Box(..)
    , BoxState(..)
    )
where

-- import Control.Lens
import Control.Monad.State
import Object
import Sprite
import System.Message

data BoxState = BoxOpened | BoxClosed

data Box = MkBox
    { _boxState  :: BoxState
    , _boxLocked :: Bool
    }

-- Lenses
{-
boxState :: Lens' Box BoxState
boxLocked :: Lens' Box Bool
boxState f x = (\s -> x { _boxState = s }) <$> (f $ _boxState x)
boxLocked f x = (\s -> x { _boxLocked = s }) <$> (f $ _boxLocked x)
-}

boxObject :: Object -> Box -> Object
boxObject obj box = obj
    { objSolid  = True
    , objSprite = boxSprite box
    , objMsg    = \msg -> boxObject obj <$> runState (boxMsg msg) box
    }

defaultBox :: Box
defaultBox = MkBox 
    { _boxState  = BoxOpened
    , _boxLocked = False
    }

boxSprite :: Box -> Sprite
boxSprite box = case _boxState box of
    BoxOpened -> sprite 0 2
    BoxClosed -> sprite 0 1

boxMsg :: Message -> State Box [Message]
boxMsg m = do
    case msgType m of
        -- InteractMsg -> boxInteract
        _           -> return []

{-
boxInteract :: State Box [Message]
boxInteract = do
    box <- get
    if box ^. boxLocked then
        return ()
    else
        case box ^. boxState of
            BoxOpened -> modify $ boxState .~ BoxClosed
            BoxClosed -> modify $ boxState .~ BoxOpened
    return []
-}
