module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    , engineObjectsAt
    , engineMessage
    ) where

import qualified Assoc as A
import Camera
import Coordinate
import Control.Lens
import Control.Monad.State as S
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Game
import Message
import Object
import SDL
import Ui
import World

-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Event -> State Game Bool
engineHandleEvent event =
    case eventPayload event of
        KeyboardEvent ked -> engineHandleKeyboardEvent ked
        QuitEvent         -> return True
        _                 -> return False

-- | This function handles keyboard events in the engine
engineHandleKeyboardEvent :: KeyboardEventData -> State Game Bool
engineHandleKeyboardEvent ked = do
    player <- view gamePlayer <$> S.get
    ui     <- view gameUi     <$> S.get

    if (keymotion == Pressed) then do
        let (newUi, newKeycode, shouldHalt, systems) = uiInterceptKeycode ui keycode
        foldr withState S.get systems
        modify $ gameUi .~ newUi

        case newKeycode of
            KeycodeUp    -> modify $ gameCamera %~ cameraMove UpDirection
            KeycodeDown  -> modify $ gameCamera %~ cameraMove DownDirection
            KeycodeRight -> modify $ gameCamera %~ cameraMove RightDirection
            KeycodeLeft  -> modify $ gameCamera %~ cameraMove LeftDirection
            KeycodeW     -> modify $ gameWorld  %~ engineMoveObject UpDirection    player
            KeycodeS     -> modify $ gameWorld  %~ engineMoveObject DownDirection  player
            KeycodeA     -> modify $ gameWorld  %~ engineMoveObject LeftDirection  player
            KeycodeD     -> modify $ gameWorld  %~ engineMoveObject RightDirection player
            KeycodeR     -> modify $ gameWorld  %~ engineMessage Nothing (Just player) RotateMsg
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

-- TODO: Rewrite this now that it works
uiInterceptKeycode :: Ui -> Keycode -> (Ui, Keycode, Bool, [(Game -> Game)])
uiInterceptKeycode ui keycode = 
    if keycode == KeycodeEscape then
        (ui { _uiVisible = [] }, KeycodeUnknown, False, [])
    else
        foldr go (ui, keycode, False, []) (view uiVisible ui)
    where
        go modal (uip, kc, halt, systems) = 
            case modal of
                MkUiTypeMenu UiMenuMain ->
                    case kc of
                        KeycodeQ -> (uiMenuSwitch UiMenuQuitConfirm uip, KeycodeUnknown, halt, systems)
                        _        -> (uip, kc, halt, systems)
                MkUiTypeMenu UiMenuQuitConfirm ->
                    case kc of
                        KeycodeY -> (uip, KeycodeUnknown, True, systems)
                        KeycodeN -> (uiMenuSwitch UiMenuMain uip, KeycodeUnknown, halt, systems)
                        _        -> (uip, kc, halt, systems)
                _ -> (uip, kc, halt, systems)

-- | Sends a message. The origin and the destination can both be either an object (Just) or a system (Nothing).
engineMessage :: Maybe ObjectId -> Maybe ObjectId -> Message -> World -> World
engineMessage _ Nothing _ w = 
    -- trace ("From object id: " ++ show fromObjId) $
    -- trace ("To: System") $
    -- trace ("Message: " ++ show m) $
    w
engineMessage fromObjId (Just toObjId) m w = 
    -- trace ("From object id: " ++ show fromObjId) $
    -- trace ("To object id: " ++ show toObjId) $
    -- trace ("Message: " ++ show m) $
    case flip objMsg m <$> M.lookup toObjId (view worldObjects $ w) of
        Just (newMsgs, newObj) ->
            foldr (engineMessage (Just toObjId) fromObjId)
                  (worldObjects %~ M.insert toObjId newObj $ w)
                  newMsgs
        Nothing                -> w

-- | Gives the list of objects at a given world coordinate (regardless of their layer)
engineObjectsAt :: World -> Coordinate -> [Object]
engineObjectsAt w c = mapMaybe resolveObjectIds objectIds
    where
        resolveObjectIds :: ObjectId -> Maybe Object
        resolveObjectIds oid = M.lookup oid $ view worldObjects w
        objectIds :: [ObjectId]
        objectIds = S.toList $ A.lookup c $ view worldLayer w

-- TODO: Needs a serious rewrite eventually; e.g. giving proximity/stepped on events and so on
engineMoveObject :: Direction -> ObjectId -> World -> World
engineMoveObject direction objid w =
    if (all (==False) $ map objSolid $ engineObjectsAt w newCoordinate)
    then msgOrientation . updateCoordinate $ w
    else msgOrientation w
    where
        msgOrientation z  = engineMessage Nothing (Just objid) (MovedMsg direction) z
        updateCoordinate  = worldLayer %~ A.adjustR (coordinateMove direction) objid
        currentCoordinate = S.elemAt 0 $ A.lookupR objid (view worldLayer w)
        newCoordinate     = coordinateMove direction currentCoordinate
