{- |
Module      : Items.Weapon
Description : All weapons, implementations, and stats in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Weapon
    ( Weapon (..)
    , WeaponType (..)
    , power
    ) where

import Brick

import Draw
import Items.Material
import System.Random.Stateful

data WeaponType = Dagger | Spear
    deriving (Show, Enum, Bounded)

data Weapon = Weapon
    { material   :: !Material
    , weaponType :: !WeaponType
    }

instance Show Weapon where
    show (Weapon {material, weaponType}) = show material <> " " <> show weaponType

instance Drawable WeaponType where
    draw False Spear  = txt "🔱\b "
    draw True Spear   = txt "/ "
    draw False Dagger = txt "🗡️\b "
    draw True Dagger  = txt "- "

instance Drawable Weapon where
    draw asciiOnly (Weapon {material, weaponType}) =
        draw asciiOnly material <+> draw asciiOnly weaponType

instance Uniform WeaponType where
    uniformM = uniformEnumM

instance Uniform Weapon where
    uniformM g = Weapon <$> uniformM g <*> uniformM g

weaponBonus :: WeaponType -> Int
weaponBonus Dagger = 5
weaponBonus Spear  = 3

power :: Weapon -> Int
power (Weapon {material, weaponType}) =
    materialBonus material * weaponBonus weaponType
