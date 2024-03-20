{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}

{- |
Module      : Items
Description : Items and friends
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items where

class Show a => Item a

data SomeItem = forall a. (Item a, Show a) => SomeItem a
data Material = Stone | Iron | Diamond deriving (Show)

data Weapon
    = Sword Material
    | Spear Material

data Armour
    = Helmet Material
    | Cuirass Material
    | Gloves Material
    | Boots Material

instance Item Weapon
instance Item Armour

instance Show SomeItem where
    show (SomeItem i) = show i

instance Show Weapon where
    show (Sword material) = show material <> " sword"
    show (Spear material) = show material <> " spear"

instance Show Armour where
    show (Helmet material)  = show material <> " helmet"
    show (Cuirass material) = show material <> " cuirass"
    show (Gloves material)  = show material <> " gloves"
    show (Boots material)   = show material <> " boots"

power :: Weapon -> Int
power (Sword material) = materialBonus material * 5
power (Spear material) = materialBonus material * 3

materialBonus :: Material -> Int
materialBonus = \case
    Stone -> 5
    Iron -> 15
    Diamond -> 30

armourDefence :: Armour -> Int
armourDefence (Helmet material)  = materialBonus material * 10
armourDefence (Cuirass material) = materialBonus material * 8
armourDefence (Gloves material)  = materialBonus material * 24
armourDefence (Boots material)   = materialBonus material * 12
