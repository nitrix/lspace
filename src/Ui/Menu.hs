module Ui.Menu where

import Ui
import Game
import Control.Monad.State as S
import Control.Lens
import Data.Biapplicative
import Data.List
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
        [ "[b] Build menu (soon)"
        , "[x] Destroy mode (soon)"
        , "[i] Inventory (soon)"
        , "[q] Quit"
        ]
    UiMenuQuitConfirm ->
        [ "[y] Yes, confirm"
        , "[n] No, go back"
        ]
    UiMenuBuild ->
        [ "[b] Box (wip)"
        ]

uiMenuInterceptKeycode :: Keycode -> State Game (Keycode, Bool)
uiMenuInterceptKeycode keycode = do
    modals <- view (gameUi . uiVisible) <$> S.get
    
    results <- forM modals $ \modal -> do
        case modal of
            MkUiTypeMenu UiMenuBuild ->
                case keycode of
                    KeycodeB -> do sysWorldAddObjectAtPlayer $ boxObject defaultObject defaultBox
                                   hook $ gameUi %~ uiMenuClear
                    _        -> ignore
            MkUiTypeMenu UiMenuMain ->
                case keycode of
                    KeycodeB -> hook $ gameUi %~ uiMenuSwitch UiMenuBuild
                    KeycodeQ -> hook $ gameUi %~ uiMenuSwitch UiMenuQuitConfirm
                    _        -> ignore
            MkUiTypeMenu UiMenuQuitConfirm ->
                case keycode of
                    KeycodeY -> terminate
                    KeycodeN -> hook $ gameUi %~ uiMenuSwitch UiMenuMain
                    _        -> ignore
            _ -> ignore

    return $ foldl' (biliftA2 min (||)) (keycode, False) results

    where
        terminate = return (KeycodeUnknown, True)
        ignore    = return (keycode, False)
        hook f    = (KeycodeUnknown, False) <$ modify f
