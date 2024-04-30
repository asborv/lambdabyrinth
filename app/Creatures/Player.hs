{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Player where

import Brick (txt)
import Control.Lens (makeLenses, (%~), (&), (^.))
import Control.Lens.Combinators (to)
import Creatures.Combatant
import Draw
import Items
import World.World (Coordinate)

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

makeLenses ''Player

instance Drawable Player where
    draw asciiOnly = const $ txt symbol
      where
        symbol = if asciiOnly then ":)" else "😎"

instance Combatant Player where
    attack :: Combatant c => Player -> c -> c
    me `attack` you = you `acceptDamage` damage
      where
        classDamage = me ^. characterClass & classPower
        weaponDamage = me ^. hand . to (maybe 0 power)
        damage = classDamage + weaponDamage

    acceptDamage :: Player -> Int -> Player
    acceptDamage me damage = me & health %~ subtract damage

classPower :: Class -> Int
classPower = \case
    Wizard -> 15
    Warrior -> 85
    Rogue -> 45
