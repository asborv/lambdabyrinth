{- |
Module      : Monsters
Description : Definition of all the monsters in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Monsters where

import Creatures.Creature

data Monster = Zombie | Ghost

instance Show Monster where
    show Zombie = "🧟\b "
    show Ghost = "👻\b "

instance Creature Monster where
    attackPower :: Monster -> Int
    attackPower = \case
        Zombie -> 15
        Ghost -> 8

    defence :: Monster -> Int
    defence = \case
        Zombie -> 32
        Ghost -> 4
