module Kawaii.Assets where

import Control.Monad
import qualified SDL         as Sdl
import qualified SDL.Image   as Img
import qualified SDL.Mixer   as Mix
import qualified SDL.TTF     as Ttf
import qualified SDL.TTF.FFI as Ttf (TTFFont)
import Data.Map

type Resource = String
data Assets = Assets
    { assetsSounds   :: Map Resource Mix.Chunk
    , assetsMusic    :: Map Resource Mix.Music
    , assetsTilesets :: Map Resource Sdl.Texture
    , assetsFonts    :: Map Resource Ttf.TTFFont
    }

loadAssets :: Sdl.Renderer -> IO Assets
loadAssets renderer = do
    -- TODO: These should be loaded either automatically
    primaryTileset <- Img.loadTexture renderer "assets/tileset.png"
    terminusFont   <- Ttf.openFont "assets/terminus.ttf" 16
    -- bellSound <- Mix.load "assets/bell.wav"

    return $ Assets
        { assetsSounds   = empty
        , assetsMusic    = empty
        , assetsTilesets = fromList [("primary", primaryTileset)]
        , assetsFonts    = fromList [("terminus", terminusFont)]
        }

unloadAssets :: Assets -> IO ()
unloadAssets assets = do
    -- Destroy tilsets
    forM_ (assetsTilesets assets) Sdl.destroyTexture

    -- Free sounds and music
    forM_ (assetsMusic assets) Mix.free
    forM_ (assetsSounds assets) Mix.free

    -- Close all fonts
    forM_ (assetsFonts assets) Ttf.closeFont