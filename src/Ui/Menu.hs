module Ui.Menu where

import Ui
import Game
import Control.Monad.State as S
import Control.Lens
import SDL
import Object
import Object.Box
import System.World

uiMenuClear :: Ui -> Ui
uiMenuClear = uiVisible %~ filter (isn't _UiTypeMenu)

uiMenuSwitch :: UiTypeMenu -> Ui -> Ui
uiMenuSwitch ty ui = uiMenuClear ui & uiVisible %~ (MkUiTypeMenu ty:)

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
        , "[n] No, back"
        ]
    UiMenuBuild ->
        [ "[b] Box"
        ]

uiMenuInterceptKeycode :: Keycode -> State Game [Bool]
uiMenuInterceptKeycode keycode = do
    modals <- view (gameUi . uiVisible) <$> S.get
    
    forM modals $ \modal -> do
        case modal of
            MkUiTypeMenu UiMenuBuild ->
                case keycode of
                    KeycodeB -> False <$ do
                                sysWorldAddObjectAtPlayer $ boxObject defaultObject defaultBox
                                modify $ gameUi %~ uiMenuClear
                    _        -> return False
            MkUiTypeMenu UiMenuMain ->
                case keycode of
                    KeycodeB -> False <$ (modify $ gameUi %~ uiMenuSwitch UiMenuBuild)
                    KeycodeQ -> False <$ (modify $ gameUi %~ uiMenuSwitch UiMenuQuitConfirm)
                    _        -> return False
            MkUiTypeMenu UiMenuQuitConfirm ->
                case keycode of
                    KeycodeY -> return True
                    KeycodeN -> False <$ (modify $ gameUi %~ uiMenuSwitch UiMenuMain)
                    _        -> return False
            _ -> return False
