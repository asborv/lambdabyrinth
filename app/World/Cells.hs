{- |
Module      : World.Cells
Description : All kinds of cells - and their data - that exist in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Cells (Cell (..), isTraversible, VerticalDirection (..)) where

import Brick (txt)
import qualified Data.Text as T
import Draw

data VerticalDirection = Upwards | Downwards deriving (Eq)
data Cell
    = Door
    | Empty
    | Floor
    | Stair VerticalDirection
    | Tunnel
    | Wall
    deriving (Eq)

isTraversible :: Cell -> Bool
isTraversible Door = True
isTraversible Empty = False
isTraversible Floor = True
isTraversible (Stair _) = True
isTraversible Tunnel = True
isTraversible Wall = False

instance Show Cell where
    show (Stair Downwards) = "V "
    show (Stair Upwards) = "Λ "
    show Door = "λ "
    show Empty = "ε "
    show Floor = ". "
    show Tunnel = ". "
    show Wall = "# "

instance Drawable Cell where
    draw _ = txt . T.pack . show
