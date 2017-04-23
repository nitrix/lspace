module Kawaii.Stage
    ( Stage(Stage)
    , Updating
    , Rendering
    , Result(..)
    , drawTile
    , stageBubbleEvent
    , stageRender
    ) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified SDL as Sdl

import Kawaii.Assets
import Kawaii.Event
import Kawaii.FFI
import Kawaii.Game

data Stage c = Stage
    { stageUpdating  :: Event -> Updating c
    , stageRendering :: Rendering c
    }

data RenderContext = RenderContext
    { rcRenderer  :: Sdl.Renderer
    , rcTilesets  :: M.Map Resource Sdl.Texture
    }

-- Aliases for convenience
type Updating c = StateT c Game (Result c)
type Rendering c = ReaderT c (ReaderT RenderContext Game) () -- TODO: Please a newtype for the readerT

data Result c = Success
              | Skip
              | Switch (Stage c)
              | Bring (Stage c)
              | Destroy
              | Terminate

-- TODO: Can we make this more pretty :/ ?
-- | Bubbles a given event through a series of stateful stages whose results might
-- interfere with future stages in the current or ulterior iterations.
-- It's doing all this while also threading a custom user state `c`.
stageBubbleEvent :: Event -> c -> S.Seq (Stage c) -> Game (S.Seq (Stage c), c)
stageBubbleEvent event custom stages = go S.empty stages custom
    where
        go pastStages futureStages c
            | S.null futureStages = return (pastStages, c)
            | otherwise = do
                let currentStage = futureStages `S.index` 0
                (result, newC) <- runStateT (stageUpdating currentStage event) c
                case result of
                    Success   -> return (stages, newC)
                    Skip      -> go (pastStages S.|> currentStage) (S.drop 1 $ futureStages) newC 
                    Switch s  -> go (s S.<| pastStages) (S.drop 1 $ futureStages) newC
                    Bring s   -> go ((s S.<| pastStages) S.|> currentStage) (S.drop 1 $ futureStages) newC
                    Destroy   -> go pastStages (S.drop 1 $ futureStages) newC
                    Terminate -> do
                        gameLiftIO pushQuitEvent
                        return (S.empty, newC)

stageRender :: S.Seq (Stage c) -> Sdl.Renderer -> Assets -> GameState -> c -> IO ()
stageRender stages renderer assets gameState customState = forM_ stages $ \stage -> do
    let inner = runReaderT (stageRendering stage) customState
    let game = runReaderT inner (RenderContext renderer tilesets)
    evalGame game gameState
    where
        tilesets = assetsTilesets assets

-- TODO: Move this to the future renderer module
-- Must untangle some of the Game/Rendering/Updating/UpdateM/RenderM types.
-- TODO: Modify the final printed result based on the camera position
drawTile :: Resource -> Int -> Int -> Integer -> Integer -> Int -> Int -> Rendering c
drawTile resource tilesetX tilesetY x y offsetX offsetY = do
    renderer <- lift $ asks rcRenderer
    tilesets <- lift $ asks rcTilesets
    
    case M.lookup resource tilesets of
        Nothing -> return ()
        Just tileset -> lift $ lift $ gameLiftIO $ Sdl.copy renderer tileset (Just src) (Just dst) -- TODO: lift $ lift $ gameLiftIO, lol really.
    where
        tileSize = Sdl.V2 32 32
        src = Sdl.Rectangle (Sdl.P $ Sdl.V2 (fromIntegral tilesetX) (fromIntegral tilesetY) * tileSize) tileSize
        dst = Sdl.Rectangle (Sdl.P $ Sdl.V2 (fromIntegral x) (fromIntegral y) * tileSize + Sdl.V2 (fromIntegral offsetX) (fromIntegral offsetY)) tileSize