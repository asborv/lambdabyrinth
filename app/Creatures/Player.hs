{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}

{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Player where

import Control.Lens
import Creatures.Creature
import Items
import World (Coordinate)

data Class = Wizard | Warrior | Rogue deriving (Show)

data Player = Player
    { _name :: String
    , _pos :: Coordinate
    , _hand :: Maybe Weapon
    , _helmet :: Maybe Armour
    , _cuirass :: Maybe Armour
    , _gloves :: Maybe Armour
    , _boots :: Maybe Armour
    , _health :: Int
    , _characterClass :: Class
    }
    deriving (Show)

makeLenses ''Player

instance Creature Player where
    attackPower :: Player -> Int
    attackPower player = (player ^. characterClass & classPower) + weaponPower
      where
        weaponPower = maybe 0 power (player ^. hand)

    defence :: Player -> Int
    defence player = classDefence + armourBonus
      where
        -- Intermediate values
        helmetBonus  = player ^. helmet & maybe 0 armourDefence
        cuirassBonus = player ^. cuirass & maybe 0 armourDefence
        gloveBonus   = player ^. gloves & maybe 0 armourDefence
        bootBonus    = player ^. boots & maybe 0 armourDefence
        -- Values actually used in computation
        armourBonus  = helmetBonus + cuirassBonus + gloveBonus + bootBonus
        classDefence = player ^. characterClass & classPower

classPower :: Class -> Int
classPower = \case
    Wizard -> 15
    Warrior -> 85
    Rogue -> 45
