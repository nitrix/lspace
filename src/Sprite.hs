module Sprite where

type Sprite = [(Int, Int, Int, Int)] -- ^ relX, relY, spriteX, spriteY

sprite :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
sprite = (,,,)

defaultSprite :: Sprite
defaultSprite = [(0, 0, 0, 0)]
