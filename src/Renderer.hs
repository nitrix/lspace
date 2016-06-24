{-# LANGUAGE TupleSections #-}

module Renderer
    ( renderGame
    ) where

import Control.Lens
import Control.Monad.Reader
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Vector.Storable as VS
import Foreign.C.Types
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import SDL
import qualified SDL.Raw.Types as Srt
import qualified SDL.TTF as Ttf
import System.Random

import Camera
import qualified Grid as G
import Link
import Object
import Types.Cache
import Types.Coordinate
import Types.Environment
import Types.Game
import qualified Types.Ship as H
import Types.Ui
import Ui.Menu

renderGame :: Game -> EnvironmentT IO ()
renderGame game = do
    renderer <- asks envRenderer
    -- Let's prepare a new fresh screen
    rendererDrawColor renderer $= V4 0 0 0 0 -- black
    clear renderer
    -- Render various things
    subRenderVoid game
    subRenderWorld game
    subRenderUi game
    -- Present to the screen
    present renderer

subRenderUi :: Game -> EnvironmentT IO ()
subRenderUi game = do
    -- Information needed to render
    renderer    <- asks envRenderer
    font        <- asks envFont
    
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
    where
        V2 _ height = game ^. gameCamera . cameraWindowSize

subRenderWorld :: Game -> EnvironmentT IO ()
subRenderWorld game = do
    -- Information needed to render
    renderer        <- asks envRenderer
    tileset         <- asks envTileset
    tileSize        <- asks envTileSize
    
    ships <- lift $ catMaybes <$> mapM readLink shipLinks

    -- TODO: Abandon hopes whoever wants to update this monster
    things <- catMaybes . concat <$> (forM ships $ \ship -> do
        let (scx, scy) = view (H.shipCoordinate . coordinates) ship
        let grid       = view H.shipGrid ship
        let range      = ( cameraX - scx
                         , cameraY - scy
                         , (cameraX - scx) + (cameraCoordMaxX - cameraX)
                         , (cameraY - scy) + (cameraCoordMaxY - cameraY)
                         )
        
        forM (G.range range grid) $ \(x, y, v) -> do
            rv <- lift $ readLink v
            lift $ print rv
            return $ (\o -> (coordinate (scx+x) (scy+y), o)) <$> rv
        )

    {-
    let things = concat $
                 map (\s -> let sc = view H.shipCoordinate s in
                    map (\(x, y, o) ->
                        (coordinate (view coordinateX sc + x) (view coordinateY sc + y), o)
                    ) $ map _ $
                    G.range (
                        cameraX - view coordinateX sc,
                        cameraY - view coordinateY sc,
                        (cameraX - view coordinateX sc) + (cameraCoordMaxX - cameraX),
                        (cameraY - view coordinateY sc) + (cameraCoordMaxY - cameraY)
                    ) (view H.shipGrid s)
                 ) ships :: [(Coordinate, Object)]
    -}

    -- Collect renderables, because of zIndex
    -- TODO: We might have to take "things" large than is visible on the screen if we have very large
    -- multi-sprite objects that starts glitching the the edges of the screen.
    renderables <- concat <$> (forM things $ \(coord, obj) -> do
        forM (objSprite obj) $ \(coordSpriteRel, coordSpriteTile, zIndex) -> do
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
    
    where
        (V2 cameraCoordMaxX cameraCoordMaxY) = V2 cameraX cameraY + viewport
        viewport  = game ^. gameCamera . cameraViewport
        cameraX   = game ^. gameCamera . cameraCoordinate . coordinateX
        cameraY   = game ^. gameCamera . cameraCoordinate . coordinateY
        shipLinks = game ^. gameShips

subRenderVoid :: Game -> EnvironmentT IO ()
subRenderVoid game = do
    renderer <- asks envRenderer
    cacheRef <- asks envCacheRef
    cache    <- lift $ readIORef cacheRef

    case view cacheStars cache of
        [] -> do
            layers <- replicateM 5 $ do
                gx <- lift newStdGen
                let pointsx = randomRs (0, width) gx
                gy <- lift newStdGen
                let pointsy = randomRs (0, height) gy
                let points = map (P . uncurry V2) $ zip pointsx pointsy
                let dark   = VS.fromList $ take 700 points
                let normal = VS.fromList $ take 300 points
                let bright = VS.fromList $ take 50 points

                layer <- createTexture renderer RGBA8888 TextureAccessTarget (game ^. gameCamera . cameraWindowSize)

                rendererRenderTarget renderer $= Just layer
                rendererDrawColor renderer $= V4 0 0 0 0 -- transparent black
                clear renderer
                textureBlendMode layer $= BlendAlphaBlend
                rendererDrawColor renderer $= V4 35 35 35 200    -- white quite dark
                drawPoints renderer dark
                rendererDrawColor renderer $= V4 100 100 100 200 -- white kinda visible
                drawPoints renderer normal
                rendererDrawColor renderer $= V4 255 255 255 255 -- white too bright
                drawPoints renderer bright
                rendererRenderTarget renderer $= Nothing
                copy renderer layer Nothing Nothing
                return layer
            lift $ modifyIORef cacheRef $ cacheStars .~ layers
        layers -> do
            forM_ (zip layers [1..]) $ \(layer, n) -> do
                let x = negate (fromIntegral $ view (gameCamera.cameraCoordinate.coordinateX) game) `div` n `mod` width
                let y = negate (fromIntegral $ view (gameCamera.cameraCoordinate.coordinateY) game) `div` n `mod` height
                -- let x = negate (fromIntegral $ view (gameCamera.cameraCoordinate.coordinateX) game) * n `mod` width
                -- let y = negate (fromIntegral $ view (gameCamera.cameraCoordinate.coordinateY) game) * n `mod` height
                nicerCopy layer renderer (x-width) y          -- left
                nicerCopy layer renderer x         (y-height) -- top
                nicerCopy layer renderer x         y          -- middle
                nicerCopy layer renderer x         (y+height) -- bottom
                nicerCopy layer renderer (x+width) y          -- right
                nicerCopy layer renderer (x-width) (y-height) -- top-left
                nicerCopy layer renderer (x+width) (y-height) -- top-right
                nicerCopy layer renderer (x-width) (y+height) -- bottom-left
                nicerCopy layer renderer (x+width) (y+height) -- bottom-right
    where
        V2 width height = game ^. gameCamera . cameraWindowSize
        nicerCopy lyr rdr tx ty = copy rdr lyr Nothing $ Just $ Rectangle (P $ V2 tx ty) (V2 width height)
