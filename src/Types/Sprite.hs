module Types.Sprite
    ( Sprite
    , ZIndex(..)
    , defaultSprite
    , sprite
    , spritePart
    ) where

import Types.Coordinate

type Sprite = [SpritePart]
type SpritePart = (Coordinate, Coordinate, ZIndex)

data ZIndex = ZGround
            | ZOnGround
            | ZOnTop
            | ZInAir
            deriving (Eq, Ord)

sprite :: Integer -> Integer -> ZIndex -> Sprite
sprite x y z = [spritePart 0 0 x y z]

spritePart :: Integer -> Integer -> Integer -> Integer -> ZIndex -> SpritePart
spritePart relX relY spriteX spriteY indexZ = (coordinate relX relY, coordinate spriteX spriteY, indexZ)

defaultSprite :: Sprite
defaultSprite = sprite 0 0 ZOnGround
