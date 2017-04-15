module Kawaii.Ui where

{-
, Layout
, Widget

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
-}