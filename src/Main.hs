module Main where

import Engine
-- import Control.Concurrent
-- import Control.Monad.IO.Class

main :: IO ()
main = withEngine $ do
    app <- engineCreateApp "Lonesome Space" ModeFullscreen
    engineRunApp app [menuScene] render

menuScene :: Scene
menuScene event = return Continue

render :: Renderer
render = return ()