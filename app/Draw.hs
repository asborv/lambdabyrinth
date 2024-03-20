{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Draw
Description : Everything and anything that should be drawn to the TUI.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Draw where

import Brick
import Creatures.Monsters
import Creatures.Player
import qualified Data.Text as T
import World

class Drawable a where
    draw :: a -> Widget n

instance Drawable Monster where
    draw Zombie = txt "🧟\b "
    draw Ghost = txt "👻\b "

instance Drawable Player where
    draw = const $ txt "😎\b "

instance Drawable Cell where
    draw = txt . T.pack . show