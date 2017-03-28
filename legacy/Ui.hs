{-# LANGUAGE TemplateHaskell #-}

module Ui
    ( Ui
    , UiType(..)
    , UiTypeMenu(..)
    , defaultUi
    , uiVisible
    , _MkUiTypeMenu
    ) where

import Control.Lens

data Ui = MkUi
    { _uiVisible :: [UiType]
    }

data UiType = MkUiTypeMenu UiTypeMenu
            -- | MkUiTypeOverlay UiTypeOverlay 


data UiTypeMenu = UiMenuMain
                | UiMenuBuild
                | UiMenuQuitConfirm
                deriving Eq

makeLenses ''Ui
makePrisms ''UiType

-- data UiTypeOverlay = UiOverlayVitals -- TODO: defined but not used

defaultUi :: Ui
defaultUi = MkUi []
