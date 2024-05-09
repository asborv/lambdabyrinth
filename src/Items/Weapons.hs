{- |
Module      : Items.Weapons
Description : All weapons, implementations, and stats in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Weapons (Weapon (..), WeaponType (..), power) where

import Brick (txt, (<+>))
import Draw
import Items.Materials
import System.Random

data WeaponType = Dagger | Spear deriving (Show)
data Weapon = Weapon {weaponType :: WeaponType, material :: Material}

instance Show Weapon where
    show weapon = show (material weapon) <> " " <> show (weaponType weapon)

instance Drawable Weapon where
    draw asciiOnly weapon = draw asciiOnly (material weapon) <+> txt symbol
      where
        symbol = case (asciiOnly, weaponType weapon) of
            (False, Spear) -> "🔱\b "
            (True, Spear) -> "/ "
            (False, Dagger) -> "🗡️\b "
            (True, Dagger) -> "- "

instance Random Weapon where
    random g =
        let (isDagger, g') = random g
            (material, g'') = random g'
         in (Weapon (if isDagger then Dagger else Spear) material, g'')
    randomR _ = random

power :: Weapon -> Int
power weapon = materialBonus (material weapon) * weaponBonus
  where
    weaponBonus = case weaponType weapon of
        Dagger -> 5
        Spear -> 3
