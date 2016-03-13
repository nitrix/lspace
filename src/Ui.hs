module Ui where

import Control.Lens
import Data.List
import SDL.Input.Keyboard.Codes

data Ui = MkUi
    { _uiVisible :: [UiType]
    }

data UiType = UiMain
            | UiQuitConfirm
            deriving Eq

-- Lenses
uiVisible :: Lens' Ui [UiType]
uiVisible = lens _uiVisible (\s x -> s { _uiVisible = x })

defaultUi :: Ui
defaultUi = MkUi []

uiToggle :: UiType -> Ui -> Ui
uiToggle ty ui =
    if ty `elem` ui ^. uiVisible
    then ui & uiVisible %~ delete ty
    else ui & uiVisible %~ (ty:)

uiIsMenu :: UiType -> Bool
uiIsMenu = flip elem
    [ UiMain
    , UiQuitConfirm
    ]

uiMenuOptions :: UiType -> [String]
uiMenuOptions ty = case ty of
    UiMain ->
        [ "[b] Build menu (wip)"
        , "[i] Inventory (wip)"
        , "[q] Quit"
        , "[x] Destroy mode (wip)"
        ]
    UiQuitConfirm ->
        [ "[y] Yes, confirm"
        , "[n] No, stay"
        ]

uiInterceptKeycode :: Ui -> Keycode -> (Ui, Keycode, Bool)
uiInterceptKeycode ui keycode = 
    if keycode == KeycodeEscape then
        (ui { _uiVisible = [] }, KeycodeUnknown, False)
    else
        foldr go (ui, keycode, False) (view uiVisible ui)
    where
        go modal (ui, kc, halt) = 
            case modal of
                UiMain ->
                    case kc of
                        KeycodeQ -> (ui &~ do uiVisible %= delete UiMain
                                              uiVisible %= (UiQuitConfirm:)
                                    , KeycodeUnknown, halt
                                    )
                        _        -> (ui, kc, halt)
                UiQuitConfirm ->
                    case kc of
                        KeycodeY -> (ui, KeycodeUnknown, True)
                        KeycodeN -> (ui & uiVisible %~ delete modal & uiVisible %~ (UiMain:), KeycodeUnknown, halt)
                        _        -> (ui, KeycodeUnknown, halt)
