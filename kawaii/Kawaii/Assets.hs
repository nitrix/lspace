module Kawaii.Assets where

import Control.Monad
import Data.Map
import Data.Monoid
import qualified SDL         as Sdl
import qualified SDL.Image   as Img
import qualified SDL.Mixer   as Mix
import qualified SDL.TTF     as Ttf
import qualified SDL.TTF.FFI as Ttf (TTFFont)

type Resource = String
data Assets = Assets
    { assetsSounds   :: Map Resource Mix.Chunk
    , assetsMusic    :: Map Resource Mix.Music
    , assetsTilesets :: Map Resource Sdl.Texture
    , assetsFonts    :: Map Resource Ttf.TTFFont
    }

loadAssets :: Sdl.Renderer -> FilePath -> IO Assets
loadAssets renderer filepath = do
    -- TODO: These should be loaded automatically from their extension type
    primaryTileset <- Img.loadTexture renderer (filepath <> "/tileset.png")
    terminusFont   <- Ttf.openFont (filepath <> "/terminus.ttf") 16
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