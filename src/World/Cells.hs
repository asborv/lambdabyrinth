{- |
Module      : World.Cells
Description : All kinds of cells - and their data - that exist in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Cells (Cell (..), isTraversible, VerticalDirection (..)) where

import Brick (txt)
import Draw
import Items.Chests (Chest')

data VerticalDirection = Upwards | Downwards deriving (Eq)
data Cell
    = Door
    | Empty
    | Floor
    | Stair VerticalDirection
    | Tunnel
    | Wall
    | Chest Chest'

isTraversible :: Cell -> Bool
isTraversible Door = True
isTraversible Empty = False
isTraversible Floor = True
isTraversible (Stair _) = True
isTraversible Tunnel = True
isTraversible Wall = False
isTraversible (Chest _) = True

instance Drawable Cell where
    draw _ (Stair Downwards) = txt "V "
    draw _ (Stair Upwards) = txt "Λ "
    draw _ Door = txt "λ "
    draw _ Empty = txt "ε "
    draw _ Floor = txt ". "
    draw _ Tunnel = txt ". "
    draw _ Wall = txt "# "
    draw False (Chest _) = txt "📦 "
    draw True (Chest _) = txt "[]"
