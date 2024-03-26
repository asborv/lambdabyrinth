{- |
Module      : Draw
Description : Everything and anything that should be drawn to the TUI.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Draw where

import Brick (Widget, txt)
import Creatures.Monsters
import Creatures.Player
import qualified Data.Text as T
import Items
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

instance Drawable Weapon where
    draw (Spear material) = txt $ T.pack (show material) <> "🔱"
    draw (Dagger material) = txt $ T.pack (show material) <> "🗡️ "

instance Drawable Armour where
    draw (Helmet material) = txt $ T.pack (show material) <> "🪖 \b"
    draw (Cuirass material) = txt $ T.pack (show material) <> "🛡️️ "
    draw (Gloves material) = txt $ T.pack (show material) <> "🧤 \b"
    draw (Boots material) = txt $ T.pack (show material) <> "🥾 \b"
