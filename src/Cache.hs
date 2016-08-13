module Cache
    ( Cache(..)
    , defaultCache
    -- , cacheChunks TODO: what, this is never used?
    , cacheStars
    ) where

import Control.Lens
-- import qualified Data.Map as M
import qualified SDL as SDL
-- import Coordinate

data Cache = MkCache
    { -- _cacheChunks :: M.Map Coordinate SDL.Texture
    _cacheStars  :: [SDL.Texture]
    }

defaultCache :: Cache
defaultCache = MkCache
    { -- _cacheChunks = M.empty
    _cacheStars  = []
    }

cacheStars  :: Lens' Cache [SDL.Texture]
cacheStars  = lens _cacheStars  (\s x -> s { _cacheStars  = x })
-- cacheChunks :: Lens' Cache (M.Map Coordinate SDL.Texture)
-- cacheChunks = lens _cacheChunks (\s x -> s { _cacheChunks = x })
