module Grid where

data Grid k v = MkGrid

empty :: Grid k v
empty = MkGrid

fromList :: [(k, v)] -> Grid k v
fromList _ = empty
