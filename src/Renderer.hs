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
import Linear (V2(V2))
import Linear.Affine (Point(P))
import Object
import SDL
import qualified SDL.Raw.Types as Srt
import qualified SDL.TTF as Ttf
import Ui
import Ui.Menu
import Engine
import System.World

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
    renderer    <- asks envRenderer
    window      <- asks envWindow
    font        <- asks envFont
    V2 _ height <- SDL.get $ windowSize window
    
    forM_ (game ^. gameUi . uiVisible) $ \modalType -> do
        case modalType of
            (MkUiTypeMenu mt) -> do
                forM (zip [0..] $ reverse $ uiMenuOptions mt) $ \(row, text) -> do
                    surface <- Ttf.renderTextShaded font text (Srt.Color 255 255 255 0) (Srt.Color 0 0 0 0)
                    V2 surfaceWidth surfaceHeight <- surfaceDimensions surface
                    texture <- createTextureFromSurface renderer surface
                    freeSurface surface
                    let dst = Rectangle (P $ V2 0 (height - surfaceHeight - (row * 15))) (V2 surfaceWidth surfaceHeight)
                    copyEx renderer texture Nothing (Just dst) 0 Nothing (V2 False False)
                    destroyTexture texture
            _ -> return []

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
        forM_ (sysWorldObjectsAt world coord) $ \obj -> do
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