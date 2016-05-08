module Bimap where

import qualified Data.Map as M

data Bimap k v = MkBimap (M.Map k v) (M.Map v k)

empty :: Bimap k v
empty = MkBimap M.empty M.empty
