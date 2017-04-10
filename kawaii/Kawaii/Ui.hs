module Kawaii.Ui
    ( Result(..)
    , Ui(Ui)
    , Layout
    , Widget
    ) where

import qualified SDL as Sdl

import Kawaii.Assets
import Kawaii.Event
import Kawaii.Game
import Kawaii.Renderer

data Result = Success
            | Skip
            | Switch Ui
            | Bring Ui
            | Destroy
            | Terminate

data Ui = Ui
    { uiUpdate :: Event -> Game Result
    , uiRender :: Renderer ()
    }

type Width = Int
type Height = Int

data Layout = VLayout [Layout]
            | HLayout [Layout]
            | Widget (Maybe Width) (Maybe Height) Widget
            | Filler

data Widget = ImageWidget Resource 
            | LabelWidget Resource String

hCentered :: Layout -> Layout
hCentered x = HLayout [Filler, x, Filler]

vCentered :: Layout -> Layout
vCentered x = VLayout [Filler, x, Filler]

centered :: Layout -> Layout
centered = hCentered . vCentered

renderLayout :: Layout -> Sdl.Renderer -> Assets -> IO ()
renderLayout layout renderer assets = return ()