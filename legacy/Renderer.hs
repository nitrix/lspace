{-# LANGUAGE TupleSections #-}

module Renderer
    ( renderGame
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import SDL hiding (get)
import qualified SDL.Raw.Types as Srt
import qualified SDL.TTF as Ttf
import System.Random

import Cache
import Camera
import Coordinate
import Engine
import Environment
import Game
import qualified Grid as G
import Object
import qualified Region as R
-- import Sprite
import Ui
import Ui.Menu

renderGame :: Engine ()
renderGame = do
    -- Information needed to render
    renderer <- asks envRenderer

    -- Let's prepare a new fresh screen
    rendererDrawColor renderer $= V4 0 0 0 0 -- black
    clear renderer

    -- Render various things
    subRenderVoid
    subRenderWorld
    subRenderUi

    -- Present to the screen
    present renderer

subRenderUi :: Engine ()
subRenderUi = do
    -- Information needed to render
    renderer    <- asks envRenderer
    font        <- asks envFont
    game        <- get
    
    -- TODO: fix me
    let (V2 _ height) = game ^. gameCamera . cameraWindowSize
    
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
            -- _ -> return []

subRenderWorld :: Engine ()
subRenderWorld = do
    -- Information needed to render
    renderer    <- asks envRenderer
    tileset     <- asks envTileset
    tileSize    <- asks envTileSize
    cameraX     <- use $ gameCamera . cameraCoordinate . coordinateX
    cameraY     <- use $ gameCamera . cameraCoordinate . coordinateY
    viewport    <- use $ gameCamera . cameraViewport
    regionLinks <- use gameRegions
    
    -- TODO: fix me
    let (V2 cameraCoordMaxX cameraCoordMaxY) = V2 cameraX cameraY + (V2 fromIntegral fromIntegral <*> viewport)
    
    regions <- embedGame $ mapM gameReadLink regionLinks

    -- TODO: Abandon hopes whoever wants to update this monster
    things <- concat <$> (forM regions $ \region -> do
        let (scx, scy) = view (R.regionCoordinate . coordinates) region
        let grid       = view R.regionGrid region
        let range      = ( fromIntegral $ cameraX - scx
                         , fromIntegral $ cameraY - scy
                         , fromIntegral $ (cameraX - scx) + (cameraCoordMaxX - cameraX)
                         , fromIntegral $ (cameraY - scy) + (cameraCoordMaxY - cameraY)
                         )
        
        forM (G.range range grid) $ \(x, y, v) -> do
            rv <- embedGame $ gameReadLink v
            return $ (\o -> (coordinate (scx + fromIntegral x) (scy + fromIntegral y), o)) $ rv
        )

    -- Collect renderables, because of zIndex
    -- TODO: We might have to take "things" large than is visible on the screen if we have very large
    -- multi-sprite objects that starts glitching the the edges of the screen.
    renderables <- concat <$> (forM things $ \(coord, obj) -> do

        {-
        let perimeterSprite = case view objFloodFill obj of
                                0 -> spritePart 0 0 0 4 ZInAir
                                _ -> spritePart 0 0 0 3 ZInAir
        -}
        -- let spriteParts = perimeterSprite : objSprite obj
        let spriteParts = objSprite obj

        forM spriteParts $ \(coordSpriteRel, coordSpriteTile, zIndex) -> do
            -- Bunch of positions to calculate
            let srcTileX    = fromIntegral $ coordSpriteTile ^. coordinateX
            let srcTileY    = fromIntegral $ coordSpriteTile ^. coordinateY
            let dstRelX     = fromIntegral $ coordSpriteRel  ^. coordinateX
            let dstRelY     = fromIntegral $ coordSpriteRel  ^. coordinateY
            let dstTileRelX = fromIntegral $ coord           ^. coordinateX - cameraX
            let dstTileRelY = fromIntegral $ coord           ^. coordinateY - cameraY
            
            -- Final src and dst rectangles for SDL
            let src = Rectangle (P $ (V2 (CInt srcTileX) srcTileY) * fromIntegral tileSize) (fromIntegral tileSize)
            let dst = Rectangle (P $ V2 (CInt (dstTileRelX + dstRelX)) (dstTileRelY + dstRelY) * (fromIntegral tileSize)) (fromIntegral tileSize)
            return (Just src, Just dst, zIndex)
        )
    -- Render!
    forM_ (sortOn (\(_,_,a) -> a) renderables) $ \(src, dst, _) -> do
        copyEx renderer tileset src dst 0 Nothing (V2 False False)

subRenderVoid :: Engine ()
subRenderVoid = do
    renderer            <- asks envRenderer
    cacheRef            <- asks envCache
    cache               <- liftIO $ readIORef cacheRef
    (V2 width height)   <- use $ gameCamera . cameraWindowSize
    theCameraWindowSize <- use $ gameCamera . cameraWindowSize
    (cameraX, cameraY)  <- use $ gameCamera . cameraCoordinate . coordinates

    -- TODO: ugly
    let nicerCopy lyr rdr tx ty = copy rdr lyr Nothing $ Just $ Rectangle (P $ V2 tx ty) (V2 width height)

    case view cacheStars cache of
        [] -> do
            layers <- replicateM 5 $ liftIO $ do
                gx <- newStdGen
                let pointsx = randomRs (0, width) gx
                gy <- newStdGen
                let pointsy = randomRs (0, height) gy
                let points = map (P . uncurry V2) $ zip pointsx pointsy
                let dark   = VS.fromList $ take 700 points
                let normal = VS.fromList $ take 300 points
                let bright = VS.fromList $ take 50 points

                layer <- createTexture renderer RGBA8888 TextureAccessTarget theCameraWindowSize

                rendererRenderTarget renderer $= Just layer
                rendererDrawColor renderer $= V4 0 0 0 0 -- transparent black
                clear renderer
                textureBlendMode layer $= BlendAdditive -- BlendAlphaBlend
                rendererDrawColor renderer $= V4 35 35 35 200    -- white quite dark
                drawPoints renderer dark
                rendererDrawColor renderer $= V4 100 100 100 200 -- white kinda visible
                drawPoints renderer normal
                rendererDrawColor renderer $= V4 255 255 255 255 -- white too bright
                drawPoints renderer bright
                rendererRenderTarget renderer $= Nothing
                copy renderer layer Nothing Nothing
                return layer
            liftIO $ modifyIORef cacheRef $ cacheStars .~ layers
        layers -> do
            forM_ (zip layers [1..]) $ \(layer, n) -> do
                let x = negate (fromIntegral cameraX) `div` n `mod` width
                let y = negate (fromIntegral cameraY) `div` n `mod` height
                nicerCopy layer renderer (x-width) y          -- left
                nicerCopy layer renderer x         (y-height) -- top
                nicerCopy layer renderer x         y          -- middle
                nicerCopy layer renderer x         (y+height) -- bottom
                nicerCopy layer renderer (x+width) y          -- right
                nicerCopy layer renderer (x-width) (y-height) -- top-left
                nicerCopy layer renderer (x+width) (y-height) -- top-right
                nicerCopy layer renderer (x-width) (y+height) -- bottom-left
                nicerCopy layer renderer (x+width) (y+height) -- bottom-right
