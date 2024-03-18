module Creatures.Creature where

class Creature a where
    attackPower :: a -> Int
    defence :: a -> Int
