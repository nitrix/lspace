module Object.Wall
    ( wallObject
    , defaultWall
    , Wall(..)
    )
where

import Control.Monad.State
import Message
import Object
import Sprite

data Wall = MkWall
    { _wallType :: WallType
    }

data WallType = TypeHorizontal
              | TypeHorizontalBranchBottom
              | TypeHorizontalBranchTop
              | TypeVertical
              | TypeVerticalBranchLeft
              | TypeVerticalBranchRight
              | TypeCornerTopRight
              | TypeCornerTopLeft
              | TypeCornerBottomRight
              | TypeCornerBottomLeft

wallObject :: Object -> Wall -> Object
wallObject obj f = obj
    { objSolid  = True
    , objSprite = wallSprite f
    , objMsg    = \msg -> wallObject obj <$> runState (wallMsg msg) f
    }

defaultWall :: Wall
defaultWall = MkWall TypeHorizontal

wallSprite :: Wall -> Sprite
wallSprite w = case _wallType w of
    TypeHorizontal -> sprite 5 2 ZGround
    _              -> sprite 0 0 ZGround

wallMsg :: Message -> State Wall [Message]
wallMsg _ = return []
