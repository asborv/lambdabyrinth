module Creatures.Combatant where

class Combatant a where
    attack :: Combatant c => a -> c -> c
    acceptDamage :: a -> Int -> a
