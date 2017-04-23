{-# LANGUAGE TemplateHaskell #-}

module Kawaii.Camera where

import Data.Label (mkLabel)
import Data.Label.Mono (modify)
import Kawaii.Direction

data Camera = Camera
    { _cameraX :: Int
    , _cameraY :: Int
    }
mkLabel ''Camera

defaultCamera :: Camera
defaultCamera = Camera 0 0

cameraTranslate :: Int -> Direction -> Camera -> Camera
cameraTranslate delta direction camera = case direction of
    North -> modify cameraY ((+delta), camera)
    South -> modify cameraY ((+delta), camera)
    East  -> modify cameraX ((+delta), camera)
    West  -> modify cameraX ((+delta), camera)