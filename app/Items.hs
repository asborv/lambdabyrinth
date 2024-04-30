{- |
Module      : Items
Description : Items and friends
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items where

import Brick (txt)
import Draw
import Brick.Widgets.Core ((<+>))

data Material = Stone | Wood | Diamond

data Weapon
    = Dagger Material
    | Spear Material

data Armour
    = Helmet Material
    | Cuirass Material
    | Gloves Material
    | Boots Material

instance Drawable Material where
    draw True Stone = txt "🪨 \b"
    draw True Wood = txt "🪵 \b"
    draw True Diamond = txt "💎 \b"
    draw False Stone = txt "S "
    draw False Wood = txt "W "
    draw False Diamond = txt "D "

instance Drawable Weapon where
    draw asciiOnly (Spear material) = draw asciiOnly material <+> txt symbol
        where symbol = if asciiOnly then "/ " else "🔱 "
    draw asciiOnly (Dagger material) = draw asciiOnly material <+> txt symbol
        where symbol = if asciiOnly then "- " else "🗡️ "

instance Drawable Armour where
    draw asciiOnly (Helmet material) = draw asciiOnly material <+> txt symbol
        where symbol = if asciiOnly then "^ " else "🪖 "
    draw asciiOnly (Cuirass material) = draw asciiOnly material <+> txt symbol
        where symbol = if asciiOnly then "# "  else "🛡️ "
    draw asciiOnly (Gloves material) = draw asciiOnly material <+> txt symbol
        where symbol = if asciiOnly then "''"  else "🧤 "
    draw asciiOnly (Boots material) = draw asciiOnly material <+> txt symbol
        where symbol = if asciiOnly then ",,"  else "👢 "

power :: Weapon -> Int
power (Dagger material) = materialBonus material * 5
power (Spear material) = materialBonus material * 3

materialBonus :: Material -> Int
materialBonus = \case
    Wood -> 5
    Stone -> 15
    Diamond -> 30

armourDefence :: Armour -> Int
armourDefence (Helmet material) = materialBonus material * 10
armourDefence (Cuirass material) = materialBonus material * 8
armourDefence (Gloves material) = materialBonus material * 24
armourDefence (Boots material) = materialBonus material * 12
