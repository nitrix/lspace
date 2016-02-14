{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

import SDL
import qualified SDL.Image as IM
import Control.Monad
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Control.Monad.State
import Control.Lens

import Camera
import Coordinate
import Game
import World
import Object

main :: IO ()
main = do
    -- Initialize SDL an SDL_image
    initializeAll
    IM.initialize [IM.InitPNG]

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    
    -- Loading texture
    texture <- IM.loadTexture renderer "assets/tileset.png"
    -- texture <- IM.loadTexture renderer "assets/tilea4.png"
    
    -- Some options for convenience
    disableScreenSaver
    rendererDrawColor renderer $= V4 0 0 0 0 -- black

    -- Prepare game
    clear renderer
    present renderer
    showWindow window

    -- Main loop
    mainLoop window renderer texture defaultGameState
    
    -- Cleanup
    destroyRenderer renderer
    destroyWindow window
    IM.quit
    SDL.quit

mainLoop :: Window -> Renderer -> Texture -> GameState -> IO ()
mainLoop window renderer texture gameState = do
    event <- waitEvent
    
    -- Handle events
    let (halt, newGameState) = runState (gameHandleEvent event) gameState
    
    -- Render camera
    clear renderer

    V2 width height <- SDL.get $ windowSize window
    let w = newGameState ^. world
    let cameraX = newGameState ^. camera . cameraCoordinate . coordinateX
    let cameraY = newGameState ^. camera . cameraCoordinate . coordinateY
    let screenWidthInTiles = fromIntegral $ width `div` 32
    let screenHeightInTiles = fromIntegral $ height `div` 32
    
    let coordsToRender = [coordinate x y
                         | x <- [cameraX-1..cameraX+screenWidthInTiles+1]
                         , y <- [cameraY-1..cameraY+screenHeightInTiles+1]
                         ]

    let objectsToRender = concatMap (\x -> (\y -> (x, y)) <$> worldObjectsAt w x) coordsToRender
    
    mapM_ (\(coord, obj) -> do
        let tileRelX = fromIntegral $ coord ^. coordinateX - cameraX
        let tileRelY = fromIntegral $ coord ^. coordinateY - cameraY
        let (objSpriteX, objSpriteY) = objectSprite obj
        let src = Rectangle (P $ (V2 (fromIntegral objSpriteX) (fromIntegral objSpriteY)) * V2 32 32) (V2 32 32)
        let dst = Rectangle (P $ V2 (tileRelX*32) (tileRelY*32)) (V2 32 32)
        copyEx renderer texture (Just src) (Just dst) 0 Nothing (V2 False False)
        ) objectsToRender

    present renderer

    -- Do it again
    unless halt $ mainLoop window renderer texture newGameState
