{-# LANGUAGE DeriveGeneric #-}

module Types.Sprite
    ( Sprite
    , ZIndex(..)
    , defaultSprite
    , sprite
    , spritePart
    ) where

import qualified Data.Aeson as J
import GHC.Generics

import Types.Coordinate

type Sprite = [SpritePart]
type SpritePart = (Coordinate, Coordinate, ZIndex)

data ZIndex = ZGround
            | ZOnGround
            | ZOnTop
            | ZInAir
            deriving (Eq, Ord, Show, Read, Generic)

instance J.ToJSON ZIndex
instance J.FromJSON ZIndex

sprite :: Int -> Int -> ZIndex -> Sprite
sprite x y z = [spritePart 0 0 x y z]

spritePart :: Int -> Int -> Int -> Int -> ZIndex -> SpritePart
spritePart relX relY spriteX spriteY indexZ = (coordinate relX relY, coordinate spriteX spriteY, indexZ)

defaultSprite :: Sprite
defaultSprite = sprite 0 0 ZOnGround
