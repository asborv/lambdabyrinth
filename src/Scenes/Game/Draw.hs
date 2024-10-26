module Scenes.Game.Draw where

import Brick
    ( Padding (..)
    , Widget
    , hBox
    , hLimit
    , padLeft
    , txt
    , txtWrapWith
    , updateAttrMap
    , vBox
    , vLimit
    , (<+>)
    , (<=>)
    )
import Brick.AttrMap (mapAttrName)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog (Dialog, renderDialog)
import Brick.Widgets.ProgressBar (progressBar, progressCompleteAttr)
import Control.Lens ((&), (^.))
import Control.Lens.Combinators (to)
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.List (find)
import Data.List.Split
import Draw
import GHC.Arr
import Scenes.Game.Attributes
import Text.Wrap
    ( FillScope (FillAfterFirst)
    , FillStrategy (FillIndent)
    , WrapSettings (..)
    )
import Types
import World.Cells (VerticalDirection (..))
import World.Level

type Name = Bool

drawGame :: Bool -> GameState -> [Widget Name]
drawGame asciiOnly game =
    let ui =
            drawLog game
                <+> (drawLevel asciiOnly game <=> drawHealth game)
                <+> drawEquipment asciiOnly game
     in case game ^. stairConfirmation of
            Nothing -> [ui]
            Just d -> [drawConfirmationDialog d, ui]

drawConfirmationDialog :: Dialog VerticalDirection Bool -> Widget Name
drawConfirmationDialog d = renderDialog d helper
  where
    helper = vLimit 3 . center $ txt "Enter to confirm, Tab / ← / → to select options"

drawLog :: GameState -> Widget Name
drawLog game =
    let wrapSettings =
            WrapSettings True True (FillIndent 4) FillAfterFirst
     in border
            . hLimit 30
            . vBox
            $ txtWrapWith wrapSettings <$> game ^. history . to reverse

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
                | otherwise -> draw asciiOnly cell
