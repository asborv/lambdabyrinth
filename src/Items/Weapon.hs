{- |
Module      : Items.Weapon
Description : All weapons, implementations, and stats in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Weapon (Weapon (..), WeaponType (..), power) where

import Brick (txt, (<+>))

import Draw
import Items.Material
import System.Random.Stateful

data WeaponType = Dagger | Spear
  deriving (Show, Enum, Bounded)

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

instance Uniform WeaponType where
    uniformM = uniformEnumM

instance Uniform Weapon where
    uniformM g = Weapon <$> uniformM g <*> uniformM g

power :: Weapon -> Int
power weapon = materialBonus (material weapon) * weaponBonus
  where
    weaponBonus = case weaponType weapon of
        Dagger -> 5
        Spear -> 3
