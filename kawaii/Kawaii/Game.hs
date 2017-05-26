{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Kawaii.Game
    ( Game
    , GameState(..)
    , defaultGameState
    , evalGame
    , gameLiftIO
    , runGame
    , moveCamera
    ) where

import Control.Monad
import Control.Monad.State (StateT, MonadState, liftIO, runStateT, liftM)
import Data.Label (mkLabel)
import Data.Label.Monadic (modify)
import Kawaii.Camera
import Kawaii.Direction

newtype Game a = Game { unwrapGame :: StateT GameState IO a } deriving (Functor, Applicative, Monad, MonadState GameState)

data GameState = GameState
    { _gsCamera :: Camera
    }
mkLabel ''GameState

defaultGameState :: GameState
defaultGameState = GameState defaultCamera

-- This lets us lift IO operation into our Game monad,
-- yet not derive MonadIO which would give too much power to the users of this Game module/type.
gameLiftIO :: IO a -> Game a
gameLiftIO = Game . liftIO

runGame :: Game a -> GameState -> IO (a, GameState)
runGame = runStateT . unwrapGame

evalGame :: Game a -> GameState -> IO a
evalGame game gameState = liftM fst (runGame game gameState) -- TODO: So tempted to use (.:) = (.) . (.) here.

{-
stopPlayer :: Game ()
stopPlayer = do
    playerRef <- gets gamePlayer
    liftIO $ atomicModifyIORef' playerRef $ \p -> (p { playerMovingDirection = Nothing }, ())

movePlayer :: Exchange -> Direction -> Game ()
movePlayer (Exchange {..}) direction = do
    gameState <- get
    playerRef <- gets gamePlayer
    liftIO $ atomicModifyIORef' playerRef $ \p -> (p { playerMovingDirection = Just direction }, ())
    player <- liftIO $ readIORef playerRef
    if playerMoving player
    then return ()
    else do
        liftIO $ atomicModifyIORef' playerRef $ \p -> (p { playerMoving = True }, ())
        maybeAnimationTimer <- do
            if not (playerAnimating player)
            then do
                timer <- liftIO $ Sdl.addTimer 0 $ \_ -> do
                    atomicModifyIORef' playerRef $ \p -> (if playerMoving p || playerMovingDirection p /= Nothing then p { playerAnimation = let (frames, beginning) = playerAnimation p in (drop 1 frames, beginning)} else p, ())
                    writeSV gameStateSV gameState
                    player <- readIORef playerRef
                    if playerMoving player || playerMovingDirection player /= Nothing
                    then return (Sdl.Reschedule 75)
                    else return (Sdl.Cancel)
                return (Just timer)
            else return Nothing
        liftIO $ void $ Sdl.addTimer 0 $ \_ -> do
            reschedule <- atomicModifyIORef' playerRef $ \p ->
                let (offsetX, offsetY) = playerOffsetPosition p in
                let (x, y) = playerPosition p in
                let movingDirection = playerMovingDirection p in

                let delta = case direction of
                                East  -> (+8)
                                West  -> (-8)
                                North -> (+8)
                                South -> (-8)

                if offsetX /= 0 || offsetY /= 0 || movingDirection == Just direction
                then (p { playerOffsetPosition = (if offsetX == 0 then -32 + 8 else delta offsetX, if offsetY == 0 then -32 + 8 else offsetY + 8)
                        , playerPosition       = (if offsetX == 0 then x + 1 else x, if offsetY == 0 then y + 1 else y)
                        , playerMoving         = True
                        , playerAnimating      = True
                        }, Sdl.Reschedule 25)
                else (p { playerMoving = False, playerAnimating = False, playerAnimation = let (frames, beginning) = playerAnimation p in (dropWhile (/= beginning) frames, beginning) } , Sdl.Cancel)
            writeSV gameStateSV gameState
            {-
            when (reschedule == Sdl.Cancel) $ do
                case maybeAnimationTimer of 
                    Just animationTimer -> do
                        liftIO $ putStrLn "Killing previous animation thread"
                        liftIO $ void $ Sdl.removeTimer animationTimer
                    Nothing -> return ()
            -}
            return reschedule
    liftIO $ writeSV gameStateSV gameState
-}

moveCamera :: Int -> Direction -> Game ()
moveCamera delta direction = modify gsCamera (cameraTranslate delta direction)