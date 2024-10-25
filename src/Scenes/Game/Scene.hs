{- |
Module      : Scenes.Game
Description : Scene for the actual game, this is where the action happens!
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Game.Scene (playGame) where

import Brick
    ( App (..)
    , BrickEvent (VtyEvent)
    , EventM
    , attrMap
    , bg
    , defaultMain
    , fg
    , on
    , showFirstCursor
    , zoom
    )
import Brick.AttrMap (AttrMap)
import Brick.Main (halt)
import Brick.Widgets.Dialog (buttonAttr, buttonSelectedAttr, handleDialogEvent)
import Brick.Widgets.ProgressBar (progressIncompleteAttr)
import Config
import Control.Lens (use, (&), (.=), (?=), (^.), _Just)
import Control.Lens.Operators ((.~))
import qualified Creatures.Player as P
import qualified Graphics.Vty as V
import HaskellWorks.Control.Monad.Lazy (interleaveSequenceM)
import Scenes.Game.Attributes
import Scenes.Game.Draw
import Scenes.Game.Events
import Scenes.Game.Widgets (confirmationDialog)
import Types
import Utils (guarded)
import World.Cells (VerticalDirection (..))
import World.Generation (generateLevel)
import World.Level

app :: Config -> Scene GameState Name
app config@(Config {asciiOnly}) =
    App
        { appDraw = drawGame asciiOnly
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent config
        , appStartEvent = return ()
        , appAttrMap = const gameAttributes
        }

gameAttributes :: AttrMap
gameAttributes =
    attrMap
        V.defAttr
        [ (attrNameSymbol MonsterAttr, fg V.red)
        , (attrNameSymbol ChestAttr, fg V.green)
        , (attrNameSymbol WallAttr, bg $ V.rgbColor 100 100 100)
        , (attrNameSymbol LowHealthAttr, bg V.red)
        , (attrNameSymbol MediumHealthAttr, bg V.yellow)
        , (attrNameSymbol HighHealthAttr, bg V.green)
        , (progressIncompleteAttr, bg $ V.RGBColor 50 50 50)
        , (buttonAttr, V.black `on` V.white)
        , (buttonSelectedAttr, bg V.yellow)
        ]

handleEvent :: Config -> BrickEvent Name () -> EventM Name GameState ()
handleEvent config = \case
    VtyEvent e -> case e of
        V.EvKey (V.KChar 'q') [] -> halt
        V.EvKey (V.KChar 'h') [] -> stairConfirmation ?= confirmationDialog Downwards
        V.EvKey (V.KChar 'H') [] -> stairConfirmation .= Nothing
        V.EvKey V.KEnter [] -> use stairConfirmation >>= maybe (return ()) (runEvent config . confirmStairEvent)
        V.EvKey (V.KChar c) []
            | (Just direction) <- charToDirection c ->
                guarded
                    (runEvent config isPaused)
                    (runEvent config $ moveEvent direction)
        _ -> zoom (stairConfirmation . _Just) $ handleDialogEvent e
    _ -> return ()

charToDirection :: Char -> Maybe Direction
charToDirection 'w' = Just North
charToDirection 'a' = Just West
charToDirection 's' = Just South
charToDirection 'd' = Just East
charToDirection _ = Nothing

playGame :: Config -> P.Player -> IO GameState
playGame config character = do
    (level : ls) <- interleaveSequenceM $ repeat generateLevel

    let startingPosition = level ^. up
        initialState =
            GameState
                (character & P.pos .~ startingPosition)
                0
                (level : ls)
                ["Welcome to the Lambdabyrinth!"]
                Nothing

    defaultMain (app config) initialState
