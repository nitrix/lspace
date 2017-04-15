{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kawaii.Ui
    ( Result(..)
    , Ui(Ui)
    -- , Layout
    -- , Widget
    , uiHandleEvent
    ) where

import Control.Monad.State

import Kawaii.Event
import Kawaii.FFI
import Kawaii.Game
import Kawaii.Renderer
import Kawaii.Updater

data Result c = Success
              | Skip
              | Switch (Ui c)
              | Bring (Ui c)
              | Destroy
              | Terminate

data Ui c = Ui
    { uiUpdate :: Event -> Updater c Result
    , uiRender :: Renderer c ()
    }

-- TODO: unit testing this would be awesome (or refactoring it ;))
uiHandleEvent :: forall c. c -> [Ui c] -> Event -> Game (c, [Ui c])
uiHandleEvent custom allUis event = process custom allUis
    where
        -- Given a list of processed uis, uis to process, carry out updates and yield the remaining uis.
        process :: c -> [Ui c] -> Game (c, [Ui c])
        process custom [] = return (custom, allUis)
        process custom (ui:uis) = do
            (result, newC) <- runStateT (uiUpdate ui event) custom
            case result of
                Success   -> return (newC, allUis)
                Skip      -> process custom uis
                Bring  x  -> return (newC, x : allUis)
                Switch x  -> return $ (newC, let cut = length allUis - length uis - 1 in
                                      let (left, right) = splitAt cut allUis in
                                      x : left ++ drop 1 right)
                Destroy   -> return $ (newC, let cut = length allUis - length uis - 1 in
                                      let (left, right) = splitAt cut allUis in
                                      left ++ drop 1 right)
                Terminate -> gameLiftIO pushQuitEvent >> return (newC, [])
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