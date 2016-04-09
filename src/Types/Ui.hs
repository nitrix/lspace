module Types.Ui
    ( Ui
    , UiType(..)
    , UiTypeMenu(..)
    , defaultUi
    , uiVisible
    , _UiTypeMenu
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

data UiTypeOverlay = UiOverlayVitals -- TODO: defined but not used

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
