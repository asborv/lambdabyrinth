{- |
Module      : World.Cells
Description : All kinds of cells - and their data - that exist in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Cells (Cell (..), isTraversible, VerticalDirection (..)) where

import Brick (txt)
import Draw
import Items.Chest (Chest (..))
import Scenes.Game.Attributes (AttrNameSymbol (..), withSymbolAttr)

data VerticalDirection = Upwards | Downwards deriving (Eq)
data Cell
    = Floor
    | Stair VerticalDirection
    | Tunnel
    | Wall
    | Chest Chest

isTraversible :: Cell -> Bool
isTraversible Floor = True
isTraversible (Stair _) = True
isTraversible Tunnel = True
isTraversible Wall = False
isTraversible (Chest _) = True

instance Drawable Cell where
    draw _ (Stair Downwards) = txt "V "
    draw _ (Stair Upwards) = txt "Λ "
    draw _ Floor = txt "  "
    draw _ Tunnel = txt "  "
    draw _ Wall = withSymbolAttr WallAttr $ txt "  "
    draw False (Chest Open) = txt "📭\b "
    draw False (Chest (Closed _)) = txt "📫\b "
    draw True (Chest Open) = withSymbolAttr ChestAttr $ txt "()"
    draw True (Chest (Closed _)) = withSymbolAttr ChestAttr $ txt "[]"
