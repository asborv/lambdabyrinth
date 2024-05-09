{- |
Module      : Draw
Description : Everything and anything that should be drawn to the TUI.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Draw (Drawable, draw) where

import Brick (Widget)

class Drawable a where
    draw :: Bool -> a -> Widget n
