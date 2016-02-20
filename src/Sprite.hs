module Sprite where

type Sprite = [(Int, Int, Int, Int)] -- ^ relX, relY, spriteX, spriteY

sprite :: Int -> Int -> Sprite
sprite x y = [spritePart 0 0 x y]

spritePart :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
spritePart = (,,,)

defaultSprite :: Sprite
defaultSprite = sprite 0 0
