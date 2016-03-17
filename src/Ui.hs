module Ui where

import Control.Lens

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
        , "[n] No, back"
        ]
