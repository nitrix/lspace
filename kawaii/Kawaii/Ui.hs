module Kawaii.Ui
    ( Result(..)
    , Ui(Ui)
    -- , Layout
    -- , Widget
    , uiHandleEvent
    ) where

import Kawaii.Event
import Kawaii.FFI
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

-- TODO: unit testing this would be awesome (or refactoring it ;))
uiHandleEvent :: [Ui] -> Event -> Game [Ui]
uiHandleEvent allUis event = process allUis
    where
        -- Given a list of processed uis, uis to process, carry out updates and yield the remaining uis.
        process :: [Ui] -> Game [Ui]
        process [] = return allUis
        process (ui:uis) = do
            result <- uiUpdate ui event
            case result of
                Success   -> return allUis
                Skip      -> process uis
                Bring  x  -> return (x : allUis)
                Switch x  -> return $ let cut = length allUis - length uis - 1 in
                                      let (left, right) = splitAt cut allUis in
                                      x : left ++ drop 1 right
                Destroy   -> return $ let cut = length allUis - length uis - 1 in
                                      let (left, right) = splitAt cut allUis in
                                      left ++ drop 1 right
                Terminate -> gameLiftIO pushQuitEvent >> return []

{-
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