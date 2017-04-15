module Kawaii.Mixer where

import Control.Exception
import Control.Monad
import qualified Data.Map  as M
import qualified SDL       as Sdl
import qualified SDL.Mixer as Mix

import Kawaii.Assets
import Kawaii.Game

playSound :: String -> Assets -> Game ()
playSound name assets = gameLiftIO $ do 
    case M.lookup name (assetsSounds assets) of
        Just sound -> do
            -- SDL yields an exception if we try to play more sounds simultanously than we have channels available.
            -- We ignore the incident when it happens. This will result in some sounds not playing in rare noisy situations.
            void $ tryJust (\e -> case e of Sdl.SDLCallFailed _ _ _ -> Just undefined; _ -> Nothing) (Mix.play sound)
            -- Mix.playMusic {-Mix.Once-} music
        Nothing -> return ()

playMusic :: String -> Assets -> Game ()
playMusic name assets = gameLiftIO $ do 
    case M.lookup name (assetsMusic assets) of
        Just music -> Mix.playMusic Mix.Once music
        Nothing    -> return ()

mixerHalt :: IO ()
mixerHalt = do
    Mix.halt Mix.AllChannels
    Mix.haltMusic