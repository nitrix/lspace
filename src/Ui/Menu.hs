module Ui.Menu
    ( uiMenuClear
    , uiMenuSwitch
    , uiMenuInterceptKeycode
    , uiMenuOptions
    )
where

import Control.Lens
import Control.Monad.State
import Data.Biapplicative
import Data.List
import SDL

import Coordinate
import Game
import Object
import Ui
import World

-- | Clear all visible menus from a given Ui.
uiMenuClear :: Ui -> Ui
uiMenuClear = uiVisible %~ filter (isn't _MkUiTypeMenu)

-- | Hide all menus and only show the chosen one.
uiMenuSwitch :: UiTypeMenu -> Ui -> Ui
uiMenuSwitch tm ui = ui & uiMenuClear
                        & uiVisible %~ (MkUiTypeMenu tm:)

-- | All menu options
uiMenuOptions :: UiTypeMenu -> [String]
uiMenuOptions tm = case tm of
    UiMenuMain ->
        [ "[b] Build menu"
        , "[x] Destroy thing"
        , "[q] Quit"
        ]
    UiMenuQuitConfirm ->
        [ "[y] Yes, confirm"
        , "[n] No, go back"
        ]
    UiMenuBuild ->
        [ "[b] Box"
        , "[f] Floor"
        , "[w] Wall"
        ]

-- | Intercepts a Keycode from the engine and affects the game
uiMenuInterceptKeycode :: Keycode -> Game (Keycode, Bool)
uiMenuInterceptKeycode keycode = do
    modals <- use $ gameUi . uiVisible
    player <- use gamePlayer
    
    results <- forM modals $ \modal -> do
        case modal of
            MkUiTypeMenu UiMenuBuild ->
                case keycode of
                    KeycodeB -> action $ gameCreateLink defaultBox   >>= worldAtObjectAddObject player
                    KeycodeF -> action $ gameCreateLink defaultFloor >>= worldAtObjectAddObject player
                    KeycodeW -> action $ gameCreateLink defaultWall  >>= worldAtObjectAddObject player
                    _        -> ignore
            MkUiTypeMenu UiMenuMain ->
                case keycode of
                    KeycodeB -> switch UiMenuBuild
                    KeycodeQ -> switch UiMenuQuitConfirm
                    {-
                    KeycodeC -> action $ forM_ regions $ \regionLink -> do
                        region <- gameReadLink regionLink
                        forM (G.toList (view regionGrid region)) $ \(_, _, objLink) -> do
                            gameModifyLink objLink (objFloodFill .~ 0)
                    -}
                    {-
                    KeycodeF -> action $ do
                        facing     <- view objFacing     <$> gameReadLink player
                        regionLink <- view objRegion     <$> gameReadLink player
                        coord      <- view objCoordinate <$> gameReadLink player
                        region     <- gameReadLink regionLink
                        worldRegionFloodFill 5 (coordinateMove facing coord) region (const $ return ())
                    -}
                    KeycodeX -> action $ do
                        facing   <- view objFacing <$> gameReadLink player
                        location <- worldObjectLocation player
                        objects  <- worldObjectsAtLocation (coordinateMove facing location)
                        traverse worldRemoveObject objects
                    _        -> ignore
            MkUiTypeMenu UiMenuQuitConfirm ->
                case keycode of
                    KeycodeY -> terminate
                    KeycodeN -> switch UiMenuMain
                    _        -> ignore
            -- _ -> ignore

    -- Fold results by keeping the `min` of all the `fst` and `||` of all the `snd`
    return $ foldl' (biliftA2 min (||)) (keycode, False) results

    where
        -- Trusty convenient helpers to give the intercepted keycode the desired behavior
        terminate = return (KeycodeUnknown, True)                                   :: Game (Keycode, Bool)
        ignore    = return (keycode, False)                                         :: Game (Keycode, Bool)
        action f  = f >> clearAll                                                   :: Game (Keycode, Bool)
        switch tm = (KeycodeUnknown, False) <$ (modify $ gameUi %~ uiMenuSwitch tm) :: Game (Keycode, Bool)
        clearAll  = (KeycodeUnknown, False) <$ (modify $ gameUi %~ uiMenuClear)     :: Game (Keycode, Bool)
