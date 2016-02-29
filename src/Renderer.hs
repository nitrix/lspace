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
renderGame :: Game -> Environment IO ()
renderGame game = do
    renderer <- asks envRenderer
    window   <- asks envWindow
    tileset  <- asks envTileset

    -- Let's prepare a new fresh screen
    clear renderer

    V2 width height <- SDL.get $ windowSize window
    
    let screenWidthInTiles = fromIntegral $ width `div` 32
    let screenHeightInTiles = fromIntegral $ height `div` 32
    
    let cameraX = game ^. gameCamera . cameraCoordinate . coordinateX
    let cameraY = game ^. gameCamera . cameraCoordinate . coordinateY
    
    let coordsToRender = [coordinate x y
                         | x <- [cameraX-1..cameraX+screenWidthInTiles+1]
                         , y <- [cameraY-1..cameraY+screenHeightInTiles+1]
                         ]

    let thingsToRender = let objects    = concatMap getObjects coordsToRender
                             getObjects = (\coord -> (\obj -> (coord, obj)) <$> worldObjectsAt (game ^. gameWorld) coord)
                         in
                             objects
    
    mapM_ (\(coord, obj) -> do
        let tileRelX = fromIntegral $ coord ^. coordinateX - cameraX
        let tileRelY = fromIntegral $ coord ^. coordinateY - cameraY
        mapM (\sprite -> do
            let (spriteRelCoord, spriteCoord) = sprite
            let spriteRelX = fromInteger $ spriteRelCoord ^. coordinateX
            let spriteRelY = fromInteger $ spriteRelCoord ^. coordinateY
            let spriteX = fromInteger $ spriteCoord ^. coordinateX
            let spriteY = fromInteger $ spriteCoord ^. coordinateY
            let src = Rectangle (P $ (V2 (fromInteger spriteX) (fromInteger spriteY)) * V2 32 32) (V2 32 32)
            let dst = Rectangle (P $ V2 (tileRelX*32 + spriteRelX*32) (tileRelY*32 + spriteRelY*32)) (V2 32 32)
            copyEx renderer tileset (Just src) (Just dst) 0 Nothing (V2 False False)
            ) $ objSprite obj
        ) thingsToRender

    -- Render new screen
    present renderer 