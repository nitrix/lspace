module Object where

import Types.Coordinate
import Types.Object
import Types.Sprite

objSprite :: Object -> Sprite
objSprite (MkObject common (ObjectBox box)) = case _boxState box of
    BoxOpened -> sprite 0 2 ZOnGround
    BoxClosed -> sprite 0 1 ZOnGround
objSprite (MkObject common ObjectFloor) = sprite 4 1 ZGround
objSprite (MkObject common ObjectPlant) = sprite 0 1 ZOnGround
objSprite (MkObject common (ObjectPlayer player)) = case objFacing common of
    North -> sprite 1 0 ZOnTop
    South -> sprite 1 2 ZOnTop
    West  -> sprite 1 1 ZOnTop
    East  -> sprite 1 3 ZOnTop
objSprite (MkObject common (ObjectWall w)) = case _wallType w of
    WallTypeHorizontal -> sprite 5 2 ZGround
objSprite _ = defaultSprite
