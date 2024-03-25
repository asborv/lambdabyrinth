{- |
Module      : Items
Description : Items and friends
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items where

data Material = Stone | Wood | Diamond

data Weapon
    = Dagger Material
    | Spear Material

data Armour
    = Helmet Material
    | Cuirass Material
    | Gloves Material
    | Boots Material

instance Show Weapon where
    show (Dagger material) = show material <> " dagger"
    show (Spear material)  = show material <> " spear"

instance Show Armour where
    show (Helmet material)  = show material <> " helmet"
    show (Cuirass material) = show material <> " cuirass"
    show (Gloves material)  = show material <> " gloves"
    show (Boots material)   = show material <> " boots"

instance Show Material where
    show Stone   = "🪨 \b"
    show Wood    = "🪵 \b"
    show Diamond = "💎 \b"

power :: Weapon -> Int
power (Dagger material) = materialBonus material * 5
power (Spear material)  = materialBonus material * 3

materialBonus :: Material -> Int
materialBonus = \case
    Wood    -> 5
    Stone   -> 15
    Diamond -> 30

armourDefence :: Armour -> Int
armourDefence (Helmet material)  = materialBonus material * 10
armourDefence (Cuirass material) = materialBonus material * 8
armourDefence (Gloves material)  = materialBonus material * 24
armourDefence (Boots material)   = materialBonus material * 12
