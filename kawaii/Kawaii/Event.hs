module Kawaii.Event
    ( Event(..)
    , convertSdlEvent
    ) where

import qualified SDL as Sdl

-- Scancodes are physical key locations as per a QWERTY US keyboard; keycodes are virtual key mappings.
data Event = EventKeyPressed  !Sdl.Scancode !Sdl.Keycode
           | EventKeyRepeat   !Sdl.Scancode !Sdl.Keycode
           | EventKeyReleased !Sdl.Scancode !Sdl.Keycode
           | EventUnknown
           | EventQuit
           deriving (Eq, Show)

convertSdlEvent :: Sdl.EventPayload -> Event
-- Quit events
convertSdlEvent Sdl.QuitEvent = EventQuit
-- Keyboard events
convertSdlEvent (Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed True (Sdl.Keysym scancode keycode _))) = EventKeyRepeat scancode keycode
convertSdlEvent (Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed False (Sdl.Keysym scancode keycode _))) = EventKeyPressed scancode keycode
convertSdlEvent (Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Released _ (Sdl.Keysym scancode keycode _))) = EventKeyReleased scancode keycode
-- Unknown events
convertSdlEvent _ = EventUnknown