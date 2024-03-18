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

data Class = Wizard | Warrior | Rogue

data Player = Player
    { _name :: String
    , _pos :: Coordinate
    , _hand :: Maybe Item
    , _helmet :: Maybe Item
    , _cuirass :: Maybe Item
    , _gloves :: Maybe Item
    , _boots :: Maybe Item
    , _inventory :: [Item]
    , _health :: Int
    , _characterClass :: Class
    }

makeLenses ''Player

instance Show Player where
    show _ = "😎\b "

instance Creature Player where
    attackPower :: Player -> Int
    attackPower player = (player ^. characterClass & classPower) + weaponPower
      where
        weaponPower = player ^. hand & maybe 0 power

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
