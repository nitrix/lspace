module Kawaii.Stage
    ( Stage(Stage)
    , Updating
    , Rendering
    , Result(..)
    , stageBubbleEvent
    ) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Sequence as S
import qualified SDL as Sdl

import Kawaii.Event
import Kawaii.FFI
import Kawaii.Game

data Stage c = Stage
    { stageUpdating  :: Event -> Updating c
    , stageRendering :: Rendering c
    }

data RenderContext = RenderContext
    { rcRenderer  :: Sdl.Renderer
    , rcGameState :: GameState
    }

-- Aliases for convenience
type Updating c = StateT c Game (Result c)
type Rendering c = StateT c (ReaderT RenderContext IO) () -- TODO: Hide this behind a newtype so that the user doesn't have access to IO

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