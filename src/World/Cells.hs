{- |
Module      : World.Cells
Description : All kinds of cells - and their data - that exist in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Cells
    ( Cell (..)
    , isTraversible
    , VerticalDirection (..)
    ) where

import Brick (txt)

import Draw
import Items.Chest
import Scenes.Game.Attributes

data VerticalDirection = Upwards | Downwards deriving (Eq)

data Cell
    = Floor
    | Stair !VerticalDirection
    | Tunnel
    | Wall
    | Chest !Chest

isTraversible :: Cell -> Bool
isTraversible Floor     = True
isTraversible (Stair _) = True
isTraversible Tunnel    = True
isTraversible Wall      = False
isTraversible (Chest _) = True

instance Drawable Cell where
    draw _         (Stair Downwards) = txt "V "
    draw _         (Stair Upwards)   = txt "Λ "
    draw _         Floor             = txt ". "
    draw _         Tunnel            = txt ". "
    draw _         Wall              = withSymbolAttr WallAttr $ txt "  "
    draw asciiOnly (Chest c)         = draw asciiOnly c
