module Renderer
    ( renderGame
    , renderUi
    ) where

import Camera
import Control.Lens
import Control.Monad.Reader
import Coordinate
import Environment
import Game
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Object
import SDL
import qualified SDL.Raw.Types as Srt
import qualified SDL.TTF as Ttf
import Ui
import World

-- TODO: to refactor most of this, please
renderGame :: Game -> EnvironmentT IO ()
renderGame game = do
    renderer <- asks envRenderer
    -- Let's prepare a new fresh screen
    clear renderer
    -- Render various things
    renderWorld game
    renderUi game
    -- Present to the screen
    present renderer

renderUi :: Game -> EnvironmentT IO ()
renderUi game = do
    -- Information needed to render
    renderer        <- asks envRenderer
    window          <- asks envWindow
    font            <- asks envFont
    V2 width height <- SDL.get $ windowSize window
    
    rendererDrawColor renderer $= V4 255 0 0 0 -- Red 
    forM_ modals $ \modalType -> do
        case modalType of
            UiMenu -> do
                result <- createTextureFromSurface renderer =<< Ttf.renderTextSolid font "Something" (Srt.Color 255 0 0 0)
                copyEx renderer result Nothing Nothing 0 Nothing (V2 False False)
                -- fillRect renderer (Just $ Rectangle (P $ V2 100 100) (V2 100 100))

    rendererDrawColor renderer $= V4 0 0 0 0 -- Black 
    -- Present to the screen
    present renderer
    where
        modals = game ^. gameUi . uiVisible

renderWorld :: Game -> EnvironmentT IO ()
renderWorld game = do
    -- Information needed to render
    renderer        <- asks envRenderer
    window          <- asks envWindow
    tileset         <- asks envTileset
    V2 width height <- SDL.get $ windowSize window
    
    let coordinates = [ coordinate x y
                      | x <- [cameraX .. cameraX + (fromIntegral $ width `div` 32) + 1]
                      , y <- [cameraY .. cameraY + (fromIntegral $ height `div` 32) + 1]
                      ]
          
    forM_ coordinates $ \coord -> do 
        forM_ (worldObjectsAt world coord) $ \obj -> do
            forM_ (objSprite obj) $ \(coordSpriteRel, coordSpriteTile) -> do
                -- Bunch of positions to calculate
                let srcTileX    = fromInteger  $ coordSpriteTile ^. coordinateX
                let srcTileY    = fromInteger  $ coordSpriteTile ^. coordinateY
                let dstRelX     = fromInteger  $ coordSpriteRel  ^. coordinateX
                let dstRelY     = fromInteger  $ coordSpriteRel  ^. coordinateY
                let dstTileRelX = fromIntegral $ coord           ^. coordinateX - cameraX
                let dstTileRelY = fromIntegral $ coord           ^. coordinateY - cameraY
                
                -- Final src and dst rectangles for SDL
                let src = Rectangle (P $ (V2 srcTileX srcTileY) * V2 32 32) (V2 32 32)
                let dst = Rectangle (P $ V2 (dstTileRelX + dstRelX) (dstTileRelY + dstRelY) * V2 32 32) (V2 32 32)
                
                -- Render!
                copyEx renderer tileset (Just src) (Just dst) 0 Nothing (V2 False False)
    where
        cameraX  = game ^. gameCamera . cameraCoordinate . coordinateX
        cameraY  = game ^. gameCamera . cameraCoordinate . coordinateY
        world    = game ^. gameWorld
