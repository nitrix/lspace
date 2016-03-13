module Ui where

import Control.Lens
import SDL.Input.Keyboard.Codes

data Ui = MkUi
    { _uiVisible :: [UiType]
    }

data UiType = MkUiTypeMenu UiTypeMenu
            | MkUiTypeOverlay UiTypeOverlay 

data UiTypeMenu = UiMenuMain
                | UiMenuQuitConfirm
                deriving Eq

data UiTypeOverlay = UiOverlayVitals

-- Lenses
uiVisible :: Lens' Ui [UiType]
uiVisible = lens _uiVisible (\s x -> s { _uiVisible = x })

defaultUi :: Ui
defaultUi = MkUi []

{-
uiToggle :: UiType -> Ui -> Ui
uiToggle ty ui =
    if ty `elem` ui ^. uiVisible
    then ui & uiVisible %~ delete ty
    else ui & uiVisible %~ (ty:)
-}

uiMenuSwitch :: UiTypeMenu -> Ui -> Ui
uiMenuSwitch ty ui = ui &~ do
    uiVisible %= filter (not . uiIsMenu)
    uiVisible %= (MkUiTypeMenu ty:)
    where
        uiIsMenu (MkUiTypeMenu _) = True
        uiIsMenu (_) = False

uiMenuOptions :: UiTypeMenu -> [String]
uiMenuOptions ty = case ty of
    UiMenuMain ->
        [ "[b] Build menu (wip)"
        , "[x] Destroy mode (wip)"
        , "[i] Inventory (wip)"
        , "[q] Quit"
        ]
    UiMenuQuitConfirm ->
        [ "[y] Yes, confirm"
        , "[n] No, stay"
        ]

-- TODO: Rewrite this now that it works
uiInterceptKeycode :: Ui -> Keycode -> (Ui, Keycode, Bool)
uiInterceptKeycode ui keycode = 
    if keycode == KeycodeEscape then
        (ui { _uiVisible = [] }, KeycodeUnknown, False)
    else
        foldr go (ui, keycode, False) (view uiVisible ui)
    where
        go modal (uip, kc, halt) = 
            case modal of
                MkUiTypeMenu UiMenuMain ->
                    case kc of
                        KeycodeQ -> (uiMenuSwitch UiMenuQuitConfirm uip, KeycodeUnknown, halt)
                        _        -> (uip, kc, halt)
                MkUiTypeMenu UiMenuQuitConfirm ->
                    case kc of
                        KeycodeY -> (uip, KeycodeUnknown, True)
                        KeycodeN -> (uiMenuSwitch UiMenuMain uip, KeycodeUnknown, halt)
                        _        -> (uip, kc, halt)
                _ -> (uip, kc, halt)
