module Types.Cache
    ( Cache(..)
    , defaultCache
    ) where

import qualified Data.Map as M
import qualified SDL as SDL
import Types.Coordinate

data Cache = MkCache
    { _cacheChunks :: M.Map Coordinate SDL.Texture
    }

defaultCache :: Cache
defaultCache = MkCache
    { _cacheChunks = M.empty
    }
