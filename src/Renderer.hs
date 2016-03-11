module Renderer
    ( renderGame
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
import World

-- TODO: to refactor most of this, please
renderGame :: Game -> EnvironmentT IO ()
renderGame game = do
    -- Information needed to render
    renderer        <- asks envRenderer
    window          <- asks envWindow
    tileset         <- asks envTileset
    V2 width height <- SDL.get $ windowSize window
    
    -- Let's prepare a new fresh screen
    clear renderer

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

    -- Present to the screen
    present renderer
    where
        cameraX  = game ^. gameCamera . cameraCoordinate . coordinateX
        cameraY  = game ^. gameCamera . cameraCoordinate . coordinateY
        world    = game ^. gameWorld