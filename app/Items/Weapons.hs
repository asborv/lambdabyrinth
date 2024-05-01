{- |
Module      : Items.Weapons
Description : All weapons, implementations, and stats in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Weapons (Weapon (..), power) where

import Brick (txt, (<+>))
import Draw
import Items.Materials

data WeaponType = Dagger | Spear
data Weapon = Weapon {weaponType :: WeaponType, material :: Material}

instance Drawable Weapon where
    draw asciiOnly weapon = draw asciiOnly (material weapon) <+> txt symbol
      where
        symbol = case (asciiOnly, weaponType weapon) of
            (False, Spear) -> "🔱 "
            (True, Spear) -> "/ "
            (False, Dagger) -> "🗡️ "
            (True, Dagger) -> "- "

power :: Weapon -> Int
power weapon = materialBonus (material weapon) * weaponBonus
  where
    weaponBonus = case weaponType weapon of
        Dagger -> 5
        Spear -> 3
