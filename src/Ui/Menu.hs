module Ui.Menu
( uiMenuClear
, uiMenuSwitch
, uiMenuInterceptKeycode
, uiMenuOptions
)
where

import Control.Monad.State
import Control.Lens
import Data.Biapplicative
import Data.List
import SDL

import Coordinate
import Game
import qualified Grid as G
import Object
import Region
import Ui
import World

-- | Clear all visible menus from a Ui.
uiMenuClear :: Ui -> Ui
uiMenuClear = uiVisible %~ filter (isn't _MkUiTypeMenu)

-- | Remove currently visible menus and only show the new given one.
uiMenuSwitch :: UiTypeMenu -> Ui -> Ui
uiMenuSwitch tm ui = ui & uiMenuClear
                        & uiVisible %~ (MkUiTypeMenu tm:)

-- | All menu options
uiMenuOptions :: UiTypeMenu -> [String]
uiMenuOptions tm = case tm of
    UiMenuMain ->
        [ "[b] Build menu"
        , "[x] Destroy mode"
        , "[f] Test flood fill"
        , "[c] Clear flood fill"
        , "[q] Quit"
        ]
    UiMenuQuitConfirm ->
        [ "[y] Yes, confirm"
        , "[n] No, go back"
        ]
    UiMenuBuild ->
        [ "[b] Box"
        , "[f] Floor (soon)"
        , "[p] Plant (soon)"
        , "[w] Wall (soon)"
        ]

uiMenuInterceptKeycode :: Keycode -> Game (Keycode, Bool)
uiMenuInterceptKeycode keycode = do
    modals  <- gets $ view (gameUi . uiVisible)
    player  <- gets $ view gamePlayer
    regions <- gets $ view gameRegions
    
    results <- forM modals $ \modal -> do
        case modal of
            MkUiTypeMenu UiMenuBuild ->
                case keycode of
                    KeycodeB -> action $ gameCreateLink defaultBox >>= worldAtObjectAddObject player
                    KeycodeF -> ignore -- action $ sysWorldAddObjectAtPlayer $ floorObject defaultObject defaultFloor
                    KeycodeP -> ignore -- action $ sysWorldAddObjectAtPlayer $ plantObject defaultObject defaultPlant
                    KeycodeW -> ignore -- action $ sysWorldAddObjectAtPlayer $ wallObject defaultObject defaultWall
                    _        -> ignore
            MkUiTypeMenu UiMenuMain ->
                case keycode of
                    KeycodeB -> switch UiMenuBuild
                    KeycodeQ -> switch UiMenuQuitConfirm
                    KeycodeC -> action $ forM_ regions $ \regionLink -> do
                        region <- gameReadLink regionLink
                        forM (G.toList (view regionGrid region)) $ \(_, _, objLink) -> do
                            gameModifyLink objLink (objFloodFill .~ 0)
                    KeycodeF -> action $ do
                        facing     <- view objFacing     <$> gameReadLink player
                        regionLink <- view objRegion     <$> gameReadLink player
                        coord      <- view objCoordinate <$> gameReadLink player
                        region     <- gameReadLink regionLink
                        worldRegionFloodFill 5 (coordinateMove facing coord) region (const $ return ())
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
