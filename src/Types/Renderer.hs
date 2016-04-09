module Types.Renderer
    ( Renderer(..)
    , defaultRenderer
    ) where

import qualified Data.Map as M
import qualified SDL as SDL
import Types.Coordinate

data Renderer = MkRenderer
    { _rendererCache :: M.Map Coordinate SDL.Texture
    }

defaultRenderer :: Renderer
defaultRenderer = MkRenderer
    { _rendererCache = M.empty
    }
