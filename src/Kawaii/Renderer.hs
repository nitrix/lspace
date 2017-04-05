module Kawaii.Renderer where

import qualified SDL as Sdl

import Kawaii.Assets
import Kawaii.Game

renderGame :: Sdl.Renderer -> Assets -> GameState -> IO ()
renderGame renderer assets gameState = do
    -- Clear the screen
    Sdl.rendererDrawColor renderer Sdl.$= Sdl.V4 0 0 0 255
    Sdl.clear renderer

    {-
    -- Figure out where our lovely test astronaut should go
    player <- readIORef (gamePlayer gameState)
    let (x, y) = playerPosition player
    let (offsetX, offsetY) = playerOffsetPosition player
    -- Testing animation
    let src = Sdl.V2 (fromIntegral $ head $ fst $ playerAnimation player) 3
    let dst = Sdl.V2 (fromIntegral x) (fromIntegral y)
    let offsetDst = Sdl.V2 (fromIntegral offsetX) (fromIntegral offsetY)

    -- Draw our lovely test astronaut
    let tileSize = Sdl.V2 32 32
    Sdl.copy hRenderer texture
        (Just $ Sdl.Rectangle (Sdl.P $ tileSize * src) tileSize) -- source
        (Just $ Sdl.Rectangle (Sdl.P $ tileSize * dst + offsetDst) tileSize) -- destination
    -}

    -- Present the result
    Sdl.present renderer