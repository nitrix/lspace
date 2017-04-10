{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kawaii.Renderer where

newtype Renderer a = Renderer { unwrapRenderer :: IO a } deriving (Functor, Applicative, Monad)

