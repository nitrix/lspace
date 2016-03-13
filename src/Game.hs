module Game
    ( Game
    , defaultGame
    , gameCamera
    , gamePlayer
    , gameUi
    , gameWorld
    , gameHandleEvent
    , gameHandleKeyboardEvent
    ) where

import Camera
import Coordinate
import Control.Lens
import Control.Monad.State as S
import Demo
import Object
import SDL
import System.Message
import Ui
import World

-- | Contains the state of the game (things that will change over time)
data Game = MkGame
    { _gamePlayer  :: ObjectId
    , _gameCamera  :: Camera
    , _gameWorld   :: World
    , _gameUi      :: Ui
    }

-- Lenses
gameCamera :: Lens' Game Camera
gameWorld  :: Lens' Game World
gamePlayer :: Lens' Game ObjectId
gameUi     :: Lens' Game Ui
gameCamera = lens _gameCamera (\s x -> s { _gameCamera = x })
gameWorld  = lens _gameWorld  (\s x -> s { _gameWorld  = x })
gamePlayer = lens _gamePlayer (\s x -> s { _gamePlayer = x })
gameUi     = lens _gameUi     (\s x -> s { _gameUi = x })

-- | Default game state with an empty world, player and camera at 0,0
defaultGame :: Game
defaultGame = MkGame
    { _gameCamera  = defaultCamera
    , _gamePlayer  = 2 -- TODO: change back to 0
    , _gameUi      = defaultUi
    , _gameWorld   = defaultWorld &~ do
                         worldLayer   .= demoLayer
                         worldObjects .= demoObjects
    }

-- | This function takes care of all events in the game and dispatches them to the appropriate handlers.
gameHandleEvent :: Event -> State Game Bool
gameHandleEvent event =
    case eventPayload event of
        KeyboardEvent ked -> gameHandleKeyboardEvent ked
        QuitEvent         -> return True
        _                 -> return False

-- | This function handles keyboard events in the game
gameHandleKeyboardEvent :: KeyboardEventData -> State Game Bool
gameHandleKeyboardEvent ked = do
    player <- view gamePlayer <$> S.get
    ui     <- view gameUi     <$> S.get

    if (keymotion == Pressed) then do
        let (newUi, newKeycode, shouldHalt) = uiInterceptKeycode ui keycode
        modify $ gameUi .~ newUi

        case newKeycode of
            KeycodeUp    -> modify $ gameCamera %~ cameraMove UpDirection
            KeycodeDown  -> modify $ gameCamera %~ cameraMove DownDirection
            KeycodeRight -> modify $ gameCamera %~ cameraMove RightDirection
            KeycodeLeft  -> modify $ gameCamera %~ cameraMove LeftDirection
            KeycodeW     -> modify $ gameWorld  %~ worldMoveObject UpDirection    player
            KeycodeS     -> modify $ gameWorld  %~ worldMoveObject DownDirection  player
            KeycodeA     -> modify $ gameWorld  %~ worldMoveObject LeftDirection  player
            KeycodeD     -> modify $ gameWorld  %~ worldMoveObject RightDirection player
            KeycodeR     -> modify $ gameWorld  %~ worldMessage Nothing (Just player) RotateMsg
            KeycodeE     -> modify $ gameUi     %~ uiMenuSwitch UiMenuMain
            _            -> modify $ id

        return shouldHalt
    else 
        return False -- $ scancode == ScancodeEscape
    where
        keymotion   = keyboardEventKeyMotion ked -- ^ Wether the key is being pressed or released
        keysym      = keyboardEventKeysym ked    -- ^ Key symbol information: keycode or scancode representation
        keycode     = keysymKeycode keysym       -- ^ Which character is received from the operating system
        -- scancode    = keysymScancode keysym      -- ^ Physical key location as it would be on a US QWERTY keyboard
