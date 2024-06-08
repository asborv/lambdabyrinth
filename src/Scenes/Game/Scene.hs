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
    , Padding (..)
    , Widget
    , attrMap
    , bg
    , defaultMain
    , emptyWidget
    , fg
    , hBox
    , hLimit
    , on
    , padLeft
    , showFirstCursor
    , txt
    , txtWrapWith
    , updateAttrMap
    , vBox
    , vLimit
    , zoom
    , (<+>)
    , (<=>)
    )
import Brick.AttrMap (AttrMap, mapAttrName)
import Brick.Main (halt)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
    ( Dialog
    , buttonAttr
    , buttonSelectedAttr
    , dialog
    , dialogAttr
    , handleDialogEvent
    , renderDialog
    )
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.ProgressBar (progressBar, progressCompleteAttr, progressIncompleteAttr)
import Config
import Control.Lens ((%=), (&), (^.), _Just, (?=), (.=))
import Control.Lens.Combinators (to)
import Control.Lens.Operators ((.~))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT))
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.List (find)
import Data.List.Split
import Draw
import GHC.Arr
import qualified Graphics.Vty as V
import HaskellWorks.Control.Monad.Lazy (interleaveSequenceIO)
import Scenes.Game.Attributes
import Scenes.Game.Events
import Text.Wrap
    ( FillScope (FillAfterFirst)
    , FillStrategy (FillIndent)
    , WrapSettings (..)
    )
import Types
import World.Generation (generateLevel)
import World.Level

type Name = Bool

app :: Config -> Scene GameState Name
app config@(Config {asciiOnly}) =
    App
        { appDraw = drawGame asciiOnly
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent config
        , appStartEvent = return ()
        , appAttrMap = const gameAttributes
        }

drawConfirmation :: GameState -> Widget Name
drawConfirmation _ = renderDialog confirmation emptyWidget
  where
    confirmation = dialog (Just $ txt "Are you sure?") (Just (True, options)) 100
    options =
        [ ("Yes", True, True)
        , ("No", False, False)
        ]

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
        , (dialogAttr, V.white `on` V.blue)
        , (buttonAttr, V.black `on` V.white)
        , (buttonSelectedAttr, bg V.yellow)
        ]

runEvent :: Config -> GameEvent a Name -> EventM Name GameState a
runEvent config event = do
    (a, s) <- runWriterT $ runReaderT event config
    history %= (s :)
    return a

handleEvent :: Config -> BrickEvent Name () -> EventM Name GameState ()
handleEvent config = \case
    VtyEvent e -> case e of
        V.EvKey (V.KChar 'q') [] -> halt
        V.EvKey (V.KChar 'h') [] -> stairConfirmation ?= confirmationDialog
        V.EvKey (V.KChar 'H') [] -> stairConfirmation .= Nothing
        V.EvKey (V.KChar c) []
            | (Just direction) <- charToDirection c ->
                runEvent config $ moveEvent direction
        _ -> zoom (stairConfirmation . _Just) $ handleDialogEvent e
    _ -> return ()

charToDirection :: Char -> Maybe Direction
charToDirection 'w' = Just North
charToDirection 'a' = Just West
charToDirection 's' = Just South
charToDirection 'd' = Just East
charToDirection _ = Nothing

drawGame :: Bool -> GameState -> [Widget Name]
drawGame asciiOnly game =
    let ui =
            drawLog game
                <+> (drawLevel asciiOnly game <=> drawHealth game)
                <+> drawEquipment asciiOnly game
     in case game ^. stairConfirmation of
            Nothing -> [ui]
            Just d -> [renderDialog d emptyWidget, ui]

drawLog :: GameState -> Widget Name
drawLog game =
    let wrapSettings =
            WrapSettings True True (FillIndent 4) FillAfterFirst
     in border
            . hLimit 30
            . vBox
            $ txtWrapWith wrapSettings <$> game ^. history

drawHealth :: GameState -> Widget Name
drawHealth game =
    let health = game ^. player . P.health
        maxHealth = game ^. player . P.maxHealth
        healthPercent = fromIntegral health / fromIntegral maxHealth
        healthBar = progressBar (Just . show $ game ^. player . P.health) healthPercent
        healthAttr =
            if
                | healthPercent <= 0.25 -> attrNameSymbol LowHealthAttr
                | healthPercent <= 0.5 -> attrNameSymbol MediumHealthAttr
                | otherwise -> attrNameSymbol HighHealthAttr
     in border
            . vLimit 3
            . center
            . hLimit 40
            . updateAttrMap (mapAttrName healthAttr progressCompleteAttr)
            $ healthBar

drawEquipment :: Bool -> GameState -> Widget Name
drawEquipment asciiOnly game = hLimit 20 . border . vCenter $ vBox slots
  where
    slots = [handSlot, helmetSlot, cuirassSlot, glovesSlot, bootsSlot]
    handSlot = padLeft Max $ txt "\nWeapon: " <+> itemSlot (game ^. player . P.hand)
    helmetSlot = padLeft Max $ txt "\nHelmet: " <+> itemSlot (game ^. player . P.helmet)
    cuirassSlot = padLeft Max $ txt "\nCuirass: " <+> itemSlot (game ^. player . P.cuirass)
    glovesSlot = padLeft Max $ txt "\nGloves: " <+> itemSlot (game ^. player . P.gloves)
    bootsSlot = padLeft Max $ txt "\nBoots: " <+> itemSlot (game ^. player . P.boots)

    itemSlot :: Drawable a => Maybe a -> Widget Name
    itemSlot Nothing = border $ txt "    "
    itemSlot (Just item) = border (draw asciiOnly item)

drawLevel :: Bool -> GameState -> Widget Name
drawLevel asciiOnly game = borderWithLabel (txt "Lambdabyrinth") . center $ vBox (hBox <$> rows)
  where
    curr = game ^. currentLevel
    level = game ^. world . to (!! curr)
    rows = chunksOf (width level) $ do
        (coord, cell) <- level ^. cells & assocs
        let monster = find (\m -> m ^. M.position == coord) (level ^. monsters)

        return $
            if
                | game ^. player . P.pos == coord -> draw asciiOnly $ game ^. player
                | Just m <- monster -> draw asciiOnly m
                -- \| coord == level ^. up -> draw asciiOnly (Stair Upwards)
                -- \| coord == level ^. down -> draw asciiOnly (Stair Downwards)
                | otherwise -> draw asciiOnly cell

confirmationDialog :: Dialog Bool Bool
confirmationDialog =
    dialog
        (Just $ txt "Are you sure?")
        (Just (True, [("Yes", True, True), ("No", False, False)]))
        100

playGame :: P.Player -> Config -> IO GameState
playGame character config = do
    (level : ls) <- interleaveSequenceIO $ repeat generateLevel
    -- The up- and downwards stairs are guaranteed to exist on each level
    let startingPosition = level ^. up
        initialState =
            GameState
                (character & P.pos .~ startingPosition)
                0
                (level : ls)
                ["Welcome to the Lambdabyrinth!"]
                Nothing

    defaultMain (app config) initialState
