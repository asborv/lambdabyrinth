{- |
Module      : Items.Weapons
Description : All weapons, implementations, and stats in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Weapons (Weapon (..), WeaponType(..), power) where

import Brick (txt, (<+>))
import Draw
import Items.Materials

data WeaponType = Dagger | Spear deriving (Show)
data Weapon = Weapon {weaponType :: WeaponType, material :: Material} deriving (Show)

instance Drawable Weapon where
    draw asciiOnly weapon = draw asciiOnly (material weapon) <+> txt symbol
      where
        symbol = case (asciiOnly, weaponType weapon) of
            (False, Spear) -> "🔱\b "
            (True, Spear) -> "/ "
            (False, Dagger) -> "🗡️\b "
            (True, Dagger) -> "- "

power :: Weapon -> Int
power weapon = materialBonus (material weapon) * weaponBonus
  where
    weaponBonus = case weaponType weapon of
        Dagger -> 5
        Spear -> 3
