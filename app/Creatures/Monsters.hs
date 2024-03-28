{- |
Module      : Monsters
Description : Definition of all the monsters in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Monsters where

import Creatures.Combatant

data Monster = Zombie | Ghost deriving (Show)

power :: Monster -> Int
power Zombie = 32
power Ghost = 4

instance Combatant Monster where
    attack :: Combatant c => Monster -> c -> c
    me `attack` you = you `acceptDamage` power me

    acceptDamage :: Monster -> Int -> Monster
    acceptDamage _ _ = Ghost
