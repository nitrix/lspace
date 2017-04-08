module Kawaii.FFI where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

-- Raw.pushEvent is documented to be thread-safe.
-- We keep retrying on failures (every 250ms) if the event queue is full.
-- The pointer will be copied into the SDL event queue and we can immediately dispose of it after the function call.
pushQuitEvent :: IO ()
pushQuitEvent = void $ forkIO $ alloca $ \ptr -> do
    poke ptr (Raw.QuitEvent 256 0)
    void $ iterateWhile (< 0) $ do
        code <- Raw.pushEvent ptr
        threadDelay 250000
        return code