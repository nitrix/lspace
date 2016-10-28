{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine
    ( Engine
    , engineHandleEvent
    , engineHandleKeyboardEvent
    , withEngine
    
    , embedGame
    , loadGame
    , saveGame
    ) where

import Control.Lens hiding (Context)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State  (StateT(..))
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Linear (V2(V2))
import SDL

import Coordinate
import Camera
import Environment
import Game
import Link
import Ui
import Ui.Menu
import World

newtype Engine a = Engine { unwrapEngine :: EnvironmentT (MaybeT (StateT GameState IO)) a }
    deriving (Functor, Applicative, Monad, MonadState GameState, MonadReader Environment, MonadIO)

withEngine :: Engine () -> String -> Environment -> IO ()
withEngine engine name env = do
    -- Loading game
    gs <- loadGame name
    
    -- Running engine
    ngs <- fmap snd $ flip runStateT gs
                    $ runMaybeT
                    $ flip runReaderT env
                    $ unwrapEngine engine
    
    -- Save new game state
    saveGame ngs

embedGame :: Game a -> Engine a
embedGame game = Engine $ ReaderT $ \env -> MaybeT $ StateT $ \gs -> liftIO $ runGame env gs game

loadGame :: String -> IO GameState
loadGame name = do
    ctx <- initContext (Just 1000) ("data/" ++ name ++ "/")
    gs  <- readLink ctx defaultLink
    case gs of
        Just tgs -> return $ tgs & gameContext .~ ctx 
        Nothing -> error "Unable to load game state" -- TODO: should create a new game

saveGame :: GameState -> IO ()
saveGame gs = do
    writeLink ctx defaultLink gs
    saveContext ctx
    where
        ctx = view gameContext gs

-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Event -> Game Bool
engineHandleEvent event = do
    case eventPayload event of
        KeyboardEvent d      -> engineHandleKeyboardEvent d
        WindowResizedEvent d -> engineHandleWindowResizedEvent d
        QuitEvent            -> return True
        _                    -> return False

engineHandleWindowResizedEvent :: WindowResizedEventData -> Game Bool
engineHandleWindowResizedEvent wred = do
    tileSize <- gameEnv envTileSize
    let V2 width height = windowResizedEventSize wred

    modify $ gameCamera . cameraWindowSize .~ V2 (fromIntegral width) (fromIntegral height)
    modify $ gameCamera . cameraViewport .~ V2
        (fromIntegral width `div` fromIntegral tileSize)
        (fromIntegral height `div` fromIntegral tileSize)

    return False

-- | This function handles keyboard events in the engine
engineHandleKeyboardEvent :: KeyboardEventData -> Game Bool
engineHandleKeyboardEvent ked = do
    -- Modifier keys
    case keycode of
        KeycodeLShift -> modify $ gameKeyShift .~ (keymotion == Pressed)
        KeycodeRShift -> modify $ gameKeyShift .~ (keymotion == Pressed)
        KeycodeLAlt   -> modify $ gameKeyAlt   .~ (keymotion == Pressed)
        KeycodeRAlt   -> modify $ gameKeyAlt   .~ (keymotion == Pressed)
        _             -> return ()

    -- Bare keys
    if (keymotion == Pressed) then do
        -- TODO: I don't like that part
        (newKeycode, shouldHalt) <- uiMenuInterceptKeycode keycode
        if shouldHalt
        then return True
        else engineHandleBareKeycode newKeycode
    else 
        return False -- $ scancode == ScancodeEscape
    where
        keymotion   = keyboardEventKeyMotion ked -- ^ Wether the key is being pressed or released
        keysym      = keyboardEventKeysym ked    -- ^ Key symbol information: keycode or scancode representation
        keycode     = keysymKeycode keysym       -- ^ Which character is received from the operating system
        -- scancode    = keysymScancode keysym      -- ^ Physical key location as it would be on a US QWERTY keyboard

engineHandleBareKeycode :: Keycode -> Game Bool
engineHandleBareKeycode keycode = do
    player <- use gamePlayer
    shift  <- use gameKeyShift
    case keycode of
        KeycodeW       -> if shift then worldRotateObject player North else worldMoveObject player North
        KeycodeS       -> if shift then worldRotateObject player South else worldMoveObject player South
        KeycodeA       -> if shift then worldRotateObject player West  else worldMoveObject player West
        KeycodeD       -> if shift then worldRotateObject player East  else worldMoveObject player East
        -- TODO: zoom levels with caching
        -- KeycodeKPPlus  -> modify $ gameCamera %~ cameraZoom (subtract 1)
        -- KeycodeKPMinus -> modify $ gameCamera %~ cameraZoom (+1)
        KeycodeUp      -> modify $ gameCamera %~ cameraMove North
        KeycodeDown    -> modify $ gameCamera %~ cameraMove South
        KeycodeRight   -> modify $ gameCamera %~ cameraMove East
        KeycodeLeft    -> modify $ gameCamera %~ cameraMove West
        -- TODO: toggling centered camera and/or camera following when moving near edges
        -- KeycodeY       -> modify $ id -- gameCamera %~ (fromMaybe id (cameraTogglePinned <$> sysWorldCoordObjectId world player)) -- TODO: eeeww
        KeycodeE       -> modify $ gameUi     %~ uiMenuSwitch UiMenuMain
        KeycodeEscape  -> modify $ gameUi     %~ uiMenuClear
        _              -> modify $ id
    return False
