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
