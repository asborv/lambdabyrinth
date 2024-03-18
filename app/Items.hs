{-# LANGUAGE LambdaCase #-}

{- |
Module      : Items
Description : Items and friends
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items where

data ArmourPiece = Helmet | Cuirass | Gloves | Boots deriving (Show)
data WeaponType = Sword | Spear
data Material = Wood | Stone | Iron | Diamond deriving (Show)

data Item
    = Armour Material ArmourPiece
    | Weapon Material WeaponType

power :: Item -> Int
power (Weapon material weaponType) = materialBonus material * weaponBonus
  where
    weaponBonus = case weaponType of
        Sword -> 5
        Spear -> 3
power _ = 0

materialBonus :: Material -> Int
materialBonus = \case
    Wood -> 2
    Stone -> 5
    Iron -> 15
    Diamond -> 30

armorPieceBonus :: ArmourPiece -> Int
armorPieceBonus = \case
    Helmet -> 12
    Cuirass -> 24
    Gloves -> 8
    Boots -> 10

armourDefence :: Item -> Int
armourDefence (Armour material piece) = materialBonus material * armorPieceBonus piece
armourDefence _ = 0
