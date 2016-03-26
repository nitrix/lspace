module Renderer
    ( renderGame
    , renderUi
    ) where

import Camera
import Control.Lens
import Control.Monad.Reader
import Coordinate
import Data.Hash (hashInt, asWord64)
import Data.List
import qualified Data.Vector.Storable as V
import Environment
import Game
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Object
import SDL
import qualified SDL.Raw.Types as Srt
import qualified SDL.TTF as Ttf
import Ui
import Ui.Menu
import System.World

-- TODO: to refactor most of this, please
renderGame :: Game -> EnvironmentT IO ()
renderGame game = do
    renderer <- asks envRenderer
    -- Let's prepare a new fresh screen
    rendererDrawColor renderer $= V4 0 0 0 0 -- black
    clear renderer
    -- Render various things
    renderVoid game
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
    tileSize        <- asks envTileSize
    V2 width height <- SDL.get $ windowSize window
    
    let zoomLevel = game ^. gameCamera . cameraZoomLevel
    let zoomedTileSize = fromIntegral $ (iterate (`div`2) tileSize) !! zoomLevel

    let svtile    = V2 tileSize tileSize
    let dvtile    = V2 zoomedTileSize zoomedTileSize
    
    let coordinates = [ coordinate x y
                      | x <- [cameraX .. cameraX + (fromIntegral $ width `div` zoomedTileSize) + 1]
                      , y <- [cameraY .. cameraY + (fromIntegral $ height `div` zoomedTileSize) + 1]
                      ]

    -- Collect renderables, because of zIndex
    renderables <- concat . concat <$> do
        forM coordinates $ \coord -> do 
            forM (sysWorldObjectsAt world coord) $ \obj -> do
                forM (objSprite obj) $ \(coordSpriteRel, coordSpriteTile, zIndex) -> do
                    -- Bunch of positions to calculate
                    let srcTileX    = fromInteger  $ coordSpriteTile ^. coordinateX
                    let srcTileY    = fromInteger  $ coordSpriteTile ^. coordinateY
                    let dstRelX     = fromInteger  $ coordSpriteRel  ^. coordinateX
                    let dstRelY     = fromInteger  $ coordSpriteRel  ^. coordinateY
                    let dstTileRelX = fromIntegral $ coord           ^. coordinateX - cameraX
                    let dstTileRelY = fromIntegral $ coord           ^. coordinateY - cameraY
                    
                    -- Final src and dst rectangles for SDL
                    let src = Rectangle (P $ (V2 srcTileX srcTileY) * svtile) svtile
                    let dst = Rectangle (P $ V2 (dstTileRelX + dstRelX) (dstTileRelY + dstRelY) * dvtile) dvtile
                    return (Just src, Just dst, zIndex)

    -- Render!
    forM_ (sortOn (\(_,_,a) -> a) renderables) $ \(src, dst, _) -> do
        copyEx renderer tileset src dst 0 Nothing (V2 False False)
                
    where
        cameraX  = game ^. gameCamera . cameraCoordinate . coordinateX
        cameraY  = game ^. gameCamera . cameraCoordinate . coordinateY
        world    = game ^. gameWorld

renderVoid :: Game -> EnvironmentT IO ()
renderVoid game = do
    renderer        <- asks envRenderer
    window          <- asks envWindow
    V2 width height <- SDL.get $ windowSize window

    let cameraX = game ^. gameCamera . cameraCoordinate . coordinateX
    let cameraY = game ^. gameCamera . cameraCoordinate . coordinateY
        
    -- TODO 67%
    let fixedRandomPoint prlx n = P $ V2
            (fromIntegral (fromIntegral (asWord64 . hashInt $ n+(1337*prlx)) + negate cameraX * fromIntegral prlx) `mod` width)
            (fromIntegral (fromIntegral (asWord64 . hashInt $ n+(7331*prlx)) + negate cameraY * fromIntegral prlx) `mod` height)

    let points1 = V.generate 500 (fixedRandomPoint 1)
    let points2 = V.generate 200 (fixedRandomPoint 2)
    let points3 = V.generate 100 (fixedRandomPoint 3)

    rendererDrawColor renderer $= V4 85 85 85 255    -- white quite dark
    drawPoints renderer points1
    rendererDrawColor renderer $= V4 170 170 170 255 -- white kinda visible
    drawPoints renderer points2
    rendererDrawColor renderer $= V4 255 255 255 255 -- white too bright
    drawPoints renderer points3
