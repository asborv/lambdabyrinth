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
    , defaultMain
    , fg
    , hBox
    , hLimit
    , padLeft
    , txt
    , txtWrapWith
    , vBox
    , vLimit
    , (<+>)
    , (<=>)
    )
import Brick.AttrMap (AttrMap, attrName)
import Brick.Main (halt, neverShowCursor)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Config
import Control.Lens ((%=), (&), (^.))
import Control.Lens.Combinators (to)
import Control.Lens.Operators ((.~))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT))
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.List (find)
import Data.List.Split
import qualified Data.Text as T
import Draw
import GHC.Arr
import qualified Graphics.Vty as V
import HaskellWorks.Control.Monad.Lazy (interleaveSequenceIO)
import Scenes.Game.Events
import Text.Wrap
    ( FillScope (FillAfterFirst)
    , FillStrategy (FillIndent)
    , WrapSettings (..)
    )
import Types
import World.Generation (generateLevel)
import World.Level

app :: Config -> Scene GameState
app config@(Config {asciiOnly}) =
    App
        { appDraw = drawGame asciiOnly
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent config
        , appStartEvent = return ()
        , appAttrMap = const gameAttributes
        }

gameAttributes :: AttrMap
gameAttributes =
    attrMap
        V.defAttr
        [ (attrName "monster", fg V.red)
        , (attrName "chest", fg V.green)
        , (attrName "cell", fg $ V.rgbColor 150 150 150)
        ]

runEvent :: Config -> GameEvent a -> EventM Name GameState a
runEvent config event = do
    (a, s) <- runWriterT $ runReaderT event config
    history %= (s <>)
    return a

handleEvent :: Config -> BrickEvent Name () -> EventM Name GameState ()
handleEvent config = \case
    VtyEvent e -> case e of
        V.EvKey (V.KChar 'q') [] -> halt
        V.EvKey (V.KChar c) []
            | (Just direction) <- charToDirection c ->
                runEvent config $ moveEvent direction
        _ -> return ()
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
                <+> (drawLevel asciiOnly game <=> drawStats game)
                <+> drawEquipment asciiOnly game
     in [ui]

drawLog :: GameState -> Widget Name
drawLog game =
    let wrapSettings =
            WrapSettings True True (FillIndent 4) FillAfterFirst
     in border
            . hLimit 30
            . vBox
            $ txtWrapWith wrapSettings <$> game ^. history

drawStats :: GameState -> Widget Name
drawStats game =
    border
        . vLimit 3
        . center
        . txt
        . T.pack
        . show
        $ game ^. player . P.health

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
                -- | coord == level ^. up -> draw asciiOnly (Stair Upwards)
                -- | coord == level ^. down -> draw asciiOnly (Stair Downwards)
                | otherwise -> draw asciiOnly cell

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

    defaultMain (app config) initialState
