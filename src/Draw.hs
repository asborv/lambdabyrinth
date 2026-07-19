{- |
Module      : Draw
Description : Everything and anything that should be drawn to the TUI.
Maintainer  : asbjorn.orvedal@gmail.com
-}
{-# LANGUAGE RankNTypes #-}
module Draw (Drawable, draw, Glyph(..)) where

import Brick (Widget)

class Drawable a where
    draw :: Bool -> a -> Widget n

{- | Contains both ASCII-only and emoji representations of widgets.
@Drawable@ instance chooses the right based off the ASCII-only flag
-}

data Glyph = Glyph
    { emoji :: forall n. Widget n
    , ascii :: forall n. Widget n
    }

instance Drawable Glyph where
    draw False (Glyph e _) = e
    draw True  (Glyph _ a) = a
