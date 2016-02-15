{-# LANGUAGE OverloadedStrings #-}

import SDL
import qualified SDL.Image as IM
import Control.Monad
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Camera
import Coordinate
import Game
import World
import Object

type Environment m a = ReaderT Environment' m a

data Environment' = MkEnvironment
    { envGame     :: Game
    , envWindow   :: Window
    , envRenderer :: Renderer
    , envTileset  :: Texture
    }


main :: IO ()
main = do
    -- Initialize SDL an SDL_image
    initializeAll
    IM.initialize [IM.InitPNG]

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    
    -- Loading texture
    tileset <- IM.loadTexture renderer "assets/tileset.png"
    
    -- Some options for convenience
    disableScreenSaver
    rendererDrawColor renderer $= V4 0 0 0 0 -- black

    -- Prepare game
    clear renderer
    present renderer
    showWindow window

    -- Main loop
    runReaderT mainLoop $ MkEnvironment
        { envGame     = defaultGame
        , envWindow   = window
        , envRenderer = renderer
        , envTileset  = tileset
        }
    
    -- Cleanup
    destroyRenderer renderer
    destroyWindow window
    IM.quit
    SDL.quit

mainLoop :: Environment IO ()
mainLoop = do
    -- Wait for any event
    event <- waitEvent
    
    -- Handle game event
    game <- asks envGame
    let (halt, newGame) = runState (gameHandleEvent event) game
    
    -- Render world
    renderGame

    -- Do it again
    unless halt $ withReaderT (\x -> x { envGame = newGame}) mainLoop

renderGame :: Environment IO ()
renderGame = do
    game     <- asks envGame
    renderer <- asks envRenderer
    window   <- asks envWindow
    tileset  <- asks envTileset

    -- Let's prepare a new fresh screen
    clear renderer

    V2 width height <- SDL.get $ windowSize window
    
    let screenWidthInTiles = fromIntegral $ width `div` 32
    let screenHeightInTiles = fromIntegral $ height `div` 32
    
    let cameraX = game ^. camera . cameraCoordinate . coordinateX
    let cameraY = game ^. camera . cameraCoordinate . coordinateY
    
    let coordsToRender = [coordinate x y
                         | x <- [cameraX-1..cameraX+screenWidthInTiles+1]
                         , y <- [cameraY-1..cameraY+screenHeightInTiles+1]
                         ]

    let objectsToRender = concatMap (\x -> (\y -> (x, y)) <$> worldObjectsAt (game ^. world) x) coordsToRender
    
    mapM_ (\(coord, obj) -> do
        let tileRelX = fromIntegral $ coord ^. coordinateX - cameraX
        let tileRelY = fromIntegral $ coord ^. coordinateY - cameraY
        let (objSpriteX, objSpriteY) = objectSprite obj
        let src = Rectangle (P $ (V2 (fromIntegral objSpriteX) (fromIntegral objSpriteY)) * V2 32 32) (V2 32 32)
        let dst = Rectangle (P $ V2 (tileRelX*32) (tileRelY*32)) (V2 32 32)
        copyEx renderer tileset (Just src) (Just dst) 0 Nothing (V2 False False)
        ) objectsToRender

    -- Render new screen
    present renderer 
