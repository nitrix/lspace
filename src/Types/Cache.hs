{-# LANGUAGE ImpredicativeTypes #-}

module Types.Cache
    ( Cache(..)
    , defaultCache
    , cacheChunks
    , cacheStars
    ) where

import Control.Lens
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified SDL as SDL

import Types.Coordinate
import Types.Link

data Cache = MkCache
    { _cacheChunks :: M.Map Coordinate SDL.Texture
    , _cacheStars  :: [SDL.Texture]
    , _cacheLinks  :: S.Seq (forall a. Link a)
    }

defaultCache :: Cache
defaultCache = MkCache
    { _cacheChunks = M.empty
    , _cacheStars  = []
    , _cacheLinks  = S.empty
    }

cacheChunks :: Lens' Cache (M.Map Coordinate SDL.Texture)
cacheStars  :: Lens' Cache [SDL.Texture]
cacheChunks = lens _cacheChunks (\s x -> s { _cacheChunks = x })
cacheStars  = lens _cacheStars  (\s x -> s { _cacheStars  = x })
