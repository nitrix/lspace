{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Coordinate
    ( Coordinate(getCoordinate) -- TODO: ewww
    , Direction(..)
    , coordinate
    , coordinateMove
    , coordinates
    , coordinateX
    , coordinateY
    , defaultCoordinate
    ) where

import Control.Lens (Lens', lens, (%~), (&), view, set, (.~))
import Data.Aeson as J
import qualified Data.Text as T
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Prelude

data Direction = North
               | East
               | South
               | West
               deriving (Show, Read, Bounded, Enum)

instance ToJSON Direction where
    toJSON = String . T.pack . show

instance FromJSON Direction where
    parseJSON (J.String s) = return $ read $ T.unpack s
    parseJSON _ = error "Unable to parse JSON for Direction"

instance ToJSON Coordinate where
    toJSON c = String . T.pack . show $ getCoordinate c

instance FromJSON Coordinate where
    parseJSON (J.String s) = return $ read $ T.unpack s
    parseJSON _ = error "Unable to parse JSON for Coordinate"

instance ToJSON (V2 Integer) where
    toJSON (V2 x y) = object [("x", Number $ fromInteger x), ("y", Number $ fromInteger y)]

instance ToJSON (V2 Int) where
    toJSON (V2 x y) = object [("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y)]

instance FromJSON (V2 Integer) where
    parseJSON (Object o) = do
        x <- o .: "x"
        y <- o .: "y"
        return $ V2 x y

instance FromJSON (V2 Int) where
    parseJSON (Object o) = do
        x <- o .: "x"
        y <- o .: "y"
        return $ V2 x y

newtype Coordinate = Coordinate { getCoordinate :: Point V2 Int } deriving (Eq, Ord, Show, Read)

-- Lenses
coordinateX :: Lens' Coordinate Int
coordinateY :: Lens' Coordinate Int
coordinates :: Lens' Coordinate (Int, Int)
coordinateX = lens (view _x . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _x .~ z)
coordinateY = lens (view _y . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _y .~ z)
coordinates = lens (\c -> (view coordinateX c, view coordinateY c)) (\s c -> s & set coordinateX (fst c)
                                                                               & set coordinateY (snd c))

-- | Simplified Coordinate constructor
coordinate :: Int -> Int -> Coordinate
coordinate x y = Coordinate $ P $ V2 x y

-- | Compute a new coordinate relative to an existing coordinate in a given direction
coordinateMove :: Direction -> Coordinate -> Coordinate
coordinateMove North = coordinateY %~ subtract 1
coordinateMove South = coordinateY %~ (+1)
coordinateMove West  = coordinateX %~ subtract 1
coordinateMove East  = coordinateX %~ (+1)

-- | Center point
defaultCoordinate :: Coordinate
defaultCoordinate = coordinate 0 0
