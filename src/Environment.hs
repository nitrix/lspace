module Environment
    ( EnvironmentT
    , Environment(..)
    ) where

import Control.Monad.Reader
import SDL

-- | Convenience type to express computations that needs the Environment' to do their work
type EnvironmentT m a = ReaderT Environment m a

-- | Contains the assets needed to run the game (things that will not change over time)
data Environment = MkEnvironment
    { envWindow   :: Window
    , envRenderer :: Renderer
    , envTileset  :: Texture
    }
