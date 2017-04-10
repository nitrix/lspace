module Kawaii.Assets where

import Control.Monad
import Data.Map hiding (filter)
import Data.Monoid
import qualified SDL         as Sdl
import qualified SDL.Image   as Img
import qualified SDL.Mixer   as Mix
import qualified SDL.TTF     as Ttf
import qualified SDL.TTF.FFI as Ttf (TTFFont)
import System.Directory

type Resource = String
data Assets = Assets
    { assetsSounds   :: Map Resource Mix.Chunk
    , assetsMusic    :: Map Resource Mix.Music
    , assetsTilesets :: Map Resource Sdl.Texture
    , assetsFonts    :: Map Resource Ttf.TTFFont
    }

-- Automagically load assets from the given location
loadAssets :: Sdl.Renderer -> FilePath -> IO Assets
loadAssets renderer location = do
    filepaths <- fmap prependLocation <$> listDirectory location

    let soundFilepaths   = filter (extensionIs "wav") filepaths
    let tilesetFilepaths = filter (extensionIs "png") filepaths 
    let fontFilepaths    = filter (extensionIs "ttf") filepaths
    let musicFilepaths   = filter (extensionIs "mp3") filepaths

    fonts    <- forM fontFilepaths    (\x -> Ttf.openFont x 16)  -- TODO: Oh-oh, how are we going to do that font thing? Maybe they sizes are listed in the filename?
    tilesets <- forM tilesetFilepaths (Img.loadTexture renderer) -- TODO: Oh-oh, how will we distinguish tilesets from other images? Maybe the filename?
    sounds   <- forM soundFilepaths    Mix.load
    music    <- forM musicFilepaths    Mix.load

    return $ Assets
        { assetsFonts    = fromList $ zip (resourcify <$> fontFilepaths)    fonts
        , assetsMusic    = fromList $ zip (resourcify <$> musicFilepaths)   music
        , assetsSounds   = fromList $ zip (resourcify <$> soundFilepaths)   sounds
        , assetsTilesets = fromList $ zip (resourcify <$> tilesetFilepaths) tilesets
        }
    where
        prependLocation s = location <> "/" <> s
        resourcify        = takeWhile (/= '.') . reverse . takeWhile (/= '/') . reverse
        extensionIs ext   = (== '.' : ext) . reverse . take 4 . reverse

unloadAssets :: Assets -> IO ()
unloadAssets assets = do
    -- Destroy tilsets
    forM_ (assetsTilesets assets) Sdl.destroyTexture

    -- Free sounds and music
    forM_ (assetsMusic assets) Mix.free
    forM_ (assetsSounds assets) Mix.free

    -- Close all fonts
    forM_ (assetsFonts assets) Ttf.closeFont