module Ui where

import Control.Lens
import Data.List

data Ui = MkUi
    { _uiVisible :: [UiType]
    }

data UiType = UiMenu deriving Eq

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
