module Sprite
    ( Sprite
    , defaultSprite
    , sprite
    , spritePart
    ) where

import Coordinate

type Sprite = [(Coordinate, Coordinate)]

sprite :: Integer -> Integer -> Sprite
sprite x y = [spritePart 0 0 x y]

spritePart :: Integer -> Integer -> Integer -> Integer -> (Coordinate, Coordinate)
spritePart relX relY spriteX spriteY = (coordinate relX relY, coordinate spriteX spriteY)

defaultSprite :: Sprite
defaultSprite = sprite 0 0
