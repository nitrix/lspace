module Kawaii.FFI where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

-- Thread-safe. Also, we keep retrying on failures (every 250ms) if the event queue is full.
-- SDL documents that the event is copied into their queue and that we can immediately dispose of our pointer after our function call.
pushQuitEvent :: IO ()
pushQuitEvent = alloca $ \ptr -> do
    poke ptr (Raw.QuitEvent 256 0)
    void $ iterateWhile (< 0) $ do
        code <- Raw.pushEvent ptr
        threadDelay 250000
        return code