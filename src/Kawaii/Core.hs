{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kawaii.Core
    ( App(..)
    , Mode(..)
    , Result(..)
    , runApp
    ) where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Exception
import Control.Monad.Loops
import Control.Monad.State
import Data.IORef
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.Text     as T
import qualified Data.Map      as M
import qualified SDL           as Sdl
import qualified SDL.Image     as Img
import qualified SDL.Mixer     as Mix
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw
import qualified SDL.TTF       as Ttf
import qualified SDL.TTF.FFI   as Ttf (TTFFont)

import Kawaii.Ui

data Direction = North | South | East | West deriving (Eq, Show)

data ObjectPlayer = ObjectPlayer
    { playerPosition        :: (Integer, Integer)
    , playerOffsetPosition  :: (Int, Int)
    , playerMovingDirection :: Maybe Direction
    , playerMoving          :: Bool
    , playerAnimating       :: Bool
    , playerAnimation       :: ([Int], Int)
    } deriving (Show)

newtype Game a = Game { unwrapGame :: StateT GameState IO a} deriving (Functor, Applicative, Monad, MonadState GameState, MonadIO)
data GameState = GameState
    { gamePlayer :: IORef ObjectPlayer
    }

data Mode = Fullscreen | Windowed Int Int
data App = App
    { appTitle  :: String
    , appMode   :: Mode
    , appUis    :: [Ui]
    }

-- This is meant to be used preferably with the RecordWildCards extension
data Exchange = Exchange
    { eventChan    :: Chan Sdl.EventPayload
    , gameStateSV  :: MSampleVar GameState
    , mixerChan    :: Chan String
    , logicEndMVar :: MVar ()
    , hRenderer    :: Sdl.Renderer
    , audioAssets  :: M.Map String Mix.Chunk
    }

runApp :: App -> IO ()
runApp app = runInBoundThread $ do
    -- We're going to need SDL
    Sdl.initializeAll
    Img.initialize [Img.InitPNG]
    void Ttf.init
    Mix.openAudio (Mix.Audio 48000 Mix.FormatS16_LSB Mix.Mono) 1024 -- 44100 Hz

    -- Workaround the fake and real fullscreen modes not playing nice with Alt-tab
    desktopSize <- Sdl.displayBoundsSize . head <$> Sdl.getDisplays
    let windowConfig = case appMode app of
                        -- Fullscreen   -> Sdl.defaultWindow { Sdl.windowMode = Sdl.FullscreenDesktop }
                           Fullscreen   -> Sdl.defaultWindow { Sdl.windowMode = Sdl.Windowed, Sdl.windowInitialSize = desktopSize, Sdl.windowBorder = False }
                           Windowed w h -> Sdl.defaultWindow { Sdl.windowInitialSize = Sdl.V2 (fromIntegral w) (fromIntegral h) }

    -- SDL window and renderer
    window   <- Sdl.createWindow (T.pack $ appTitle app) windowConfig
    renderer <- Sdl.createRenderer window (-1) Sdl.defaultRenderer

    -- Few SDL settings; this might be a little too arbitrary
    Sdl.disableScreenSaver
    Sdl.cursorVisible Sdl.$= False
    Sdl.showWindow window
    -- Mix.setChannels 8

    -- TODO: Load music assets by wav extension and store them in map where the key is the filename
    {-
    bell <- Mix.load "bell.wav"
    let audioChunkAssets = M.empty
                         & M.insert "bell" bell
    -}
    
    defaultGameState <- GameState <$> newIORef (ObjectPlayer (0,0) (0,0) Nothing False False (cycle [1,3,2,3], 1))
    tileset          <- Img.loadTexture renderer "assets/tileset.png"
    font             <- Ttf.openFont "assets/terminus.ttf" 16

    -- Thread communication
    exchange@(Exchange {..}) <- Exchange
        <$> newChan       -- Event channel (SDL events to be processed by the logic thread)
        <*> newEmptySV    -- Game state sampling variable (contains a snapshot of the game state to render)
        <*> newChan       -- Mixer channel (how we request things to be played)
        <*> newEmptyMVar  -- Logic thread end signal
        <*> pure renderer -- Renderer
        <*> pure M.empty  -- Audio assets
    
    -- Threads
    logicThreadId   <- forkOS (logicThread exchange defaultGameState)
    renderThreadId  <- forkOS (renderThread exchange tileset font)
    networkThreadId <- forkOS (networkThread exchange)
    mixerThreadId   <- forkOS (mixerThread exchange)

    -- Main thread with event collecting
    void $ iterateWhile (/= Sdl.QuitEvent) $ do
        event <- Sdl.eventPayload <$> Sdl.waitEvent
        writeChan eventChan event
        return event
    
    -- Waiting for some important threads to finish
    readMVar logicEndMVar
    
    -- Killing all the threads
    killThread logicThreadId
    killThread renderThreadId
    killThread networkThreadId
    killThread mixerThreadId

    -- Cleanup
    -- Mix.free bell
    Sdl.destroyTexture tileset
    Ttf.closeFont font
    Ttf.quit
    Mix.closeAudio
    Sdl.quit

logicThread :: Exchange -> GameState -> IO ()
logicThread exchange@(Exchange {..}) gameState = do
    newGameState <- execStateT (unwrapGame $ gameLogic exchange) gameState
    writeSV gameStateSV newGameState
    logicThread exchange newGameState

gameLogic :: Exchange -> Game ()
gameLogic (Exchange {..}) = do
    gameState <- get
    playerRef <- gets gamePlayer
    event <- liftIO (readChan eventChan)
    case event of
        -- --------------------------- Testing movement --------------------------------
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Released False (Sdl.Keysym Sdl.ScancodeD _ _)) -> do
            liftIO $ atomicModifyIORef' playerRef $ \p -> (p { playerMovingDirection = Nothing }, ())
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed False (Sdl.Keysym Sdl.ScancodeD _ _)) -> do
            liftIO $ atomicModifyIORef' playerRef $ \p -> (p { playerMovingDirection = Just East }, ())
            player <- liftIO $ readIORef playerRef
            if playerMoving player
            then return ()
            else do
                maybeAnimationTimer <- do
                    if not (playerAnimating player)
                    then do
                        liftIO $ putStrLn "Starting animation thread"
                        timer <- liftIO $ Sdl.addTimer 0 $ \_ -> do
                            atomicModifyIORef' playerRef $ \p -> (if playerMoving p || playerMovingDirection p /= Nothing then p { playerAnimation = let (frames, beginning) = playerAnimation p in (drop 1 frames, beginning)} else p, ())
                            writeSV gameStateSV gameState
                            player <- readIORef playerRef
                            if playerMoving player || playerMovingDirection player /= Nothing
                            then return (Sdl.Reschedule 100)
                            else return (Sdl.Cancel)
                        return (Just timer)
                    else return Nothing
                liftIO $ void $ Sdl.addTimer 0 $ \_ -> do
                    reschedule <- atomicModifyIORef' playerRef $ \p ->
                        let (offsetX, offsetY) = playerOffsetPosition p in
                        let (x, y) = playerPosition p in
                        let direction =  playerMovingDirection p in

                        if offsetX /= 0 || offsetY /= 0 || direction == Just East
                        then (p { playerOffsetPosition = (if offsetX == 0 then -32 + 4 else offsetX + 4, offsetY)
                                , playerPosition       = (if offsetX == 0 then x + 1 else x, y)
                                , playerMoving         = True
                                , playerAnimating      = True
                                }, Sdl.Reschedule 50)
                        else (p { playerMoving = False, playerAnimating = False, playerAnimation = let (frames, beginning) = playerAnimation p in (dropWhile (/= beginning) frames, beginning) } , Sdl.Cancel)
                    writeSV gameStateSV gameState
                    when (reschedule == Sdl.Cancel) $ do
                        case maybeAnimationTimer of 
                            Just animationTimer -> liftIO $ void $ Sdl.removeTimer animationTimer
                            Nothing -> return ()
                    return reschedule
            liftIO $ writeSV gameStateSV gameState
            -- --------------------------- End of testing movement --------------------------------
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeEscape _ _)) -> liftIO pushQuitEvent
        -- Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeSpace _ _)) -> do
        --     writeChan mixerChan "bell"
        Sdl.QuitEvent -> liftIO $ putMVar logicEndMVar ()
        _ -> do
            return ()
            -- liftIO $ putStrLn $ takeWhile (/=' ') (show event)


renderThread :: Exchange -> Sdl.Texture -> Ttf.TTFFont  -> IO ()
renderThread (Exchange {..}) texture font = forever $ do
    -- Wait for the game state to change
    gameState <- readSV gameStateSV

    -- Clear the screen
    Sdl.rendererDrawColor hRenderer Sdl.$= Sdl.V4 0 0 0 255
    Sdl.clear hRenderer

    -- Figure out where our lovely test astronaut should go
    player <- readIORef (gamePlayer gameState)
    let (x, y) = playerPosition player
    let (offsetX, offsetY) = playerOffsetPosition player
    -- Testing animation
    let src = Sdl.V2 (fromIntegral $ head $ fst $ playerAnimation player) 3
    let dst = Sdl.V2 (fromIntegral x) (fromIntegral y)
    let offsetDst = Sdl.V2 (fromIntegral offsetX) (fromIntegral offsetY)

    -- Draw our lovely test astronaut
    let tileSize = Sdl.V2 32 32
    Sdl.copy hRenderer texture
        (Just $ Sdl.Rectangle (Sdl.P $ tileSize * src) tileSize) -- source
        (Just $ Sdl.Rectangle (Sdl.P $ tileSize * dst + offsetDst) tileSize) -- destination
    
    -- Present the result
    Sdl.present hRenderer

networkThread :: Exchange -> IO ()
networkThread _ = forever $ do
    threadDelay 1000000

mixerThread :: Exchange -> IO ()
mixerThread (Exchange {..}) = forever $ do
    key <- readChan mixerChan
    case M.lookup key audioAssets of
        Just chunk -> do
            -- SDL yields an exception if we try to play more sounds simultanously than we have channels available.
            -- We ignore the incident when it happens. This will result in some sounds not playing in rare noisy situations.
            void $ tryJust (\e -> case e of Sdl.SDLCallFailed _ _ _ -> Just undefined; _ -> Nothing) (Mix.play chunk)
            -- Mix.playMusic {-Mix.Once-} music
        Nothing -> return ()
    
-- Thread-safe. Also, we keep retrying on failures (every 250ms) if the event queue is full.
-- SDL documents that the event is copied into their queue and that we can immediately dispose of our pointer after our function call.
pushQuitEvent :: IO ()
pushQuitEvent = alloca $ \ptr -> do
    poke ptr (Raw.QuitEvent 256 0)
    void $ iterateWhile (< 0) $ do
        code <- Raw.pushEvent ptr
        threadDelay 250000
        return code