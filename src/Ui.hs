module Ui
    ( Ui
    , UiType(..)
    , UiTypeMenu(..)
    , defaultUi
    , uiVisible
    , uiMenuClear
    , uiMenuOptions
    , uiMenuSwitch
    ) where

import Control.Lens

data Ui = MkUi
    { _uiVisible :: [UiType]
    }

data UiType = MkUiTypeMenu UiTypeMenu
            | MkUiTypeOverlay UiTypeOverlay 

data UiTypeMenu = UiMenuMain
                | UiMenuBuild
                | UiMenuQuitConfirm
                deriving Eq

data UiTypeOverlay = UiOverlayVitals

-- Lenses
uiVisible :: Lens' Ui [UiType]
uiVisible = lens _uiVisible (\s x -> s { _uiVisible = x })

-- Prisms
_UiTypeMenu :: Prism' UiType UiTypeMenu
_UiTypeMenu = prism' MkUiTypeMenu $ \m -> case m of
    MkUiTypeMenu x -> Just x
    _              -> Nothing

defaultUi :: Ui
defaultUi = MkUi []

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
