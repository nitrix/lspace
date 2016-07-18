{-# LANGUAGE ExistentialQuantification #-}

module Types.Cache
    ( Cache(..)
    , defaultCache
    , cacheChunks
    , cacheLinks
    , cacheStars
    , AnyIORef(MkAnyIORef)
    ) where

import Control.Lens
import Data.IORef
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified SDL as SDL

import Types.Coordinate

data AnyIORef = forall a. MkAnyIORef {-# UNPACK #-} !(IORef a)
data Cache = MkCache
    { _cacheChunks :: M.Map Coordinate SDL.Texture
    , _cacheStars  :: [SDL.Texture]
    , _cacheLinks  :: S.Seq AnyIORef
    }

defaultCache :: Cache
defaultCache = MkCache
    { _cacheChunks = M.empty
    , _cacheStars  = []
    , _cacheLinks  = S.empty
    }

cacheChunks :: Lens' Cache (M.Map Coordinate SDL.Texture)
cacheStars  :: Lens' Cache [SDL.Texture]
cacheLinks  :: Lens' Cache (S.Seq AnyIORef)
cacheChunks = lens _cacheChunks (\s x -> s { _cacheChunks = x })
cacheStars  = lens _cacheStars  (\s x -> s { _cacheStars  = x })
cacheLinks  = lens _cacheLinks  (\s x -> s { _cacheLinks  = x })
