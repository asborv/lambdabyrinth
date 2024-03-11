{- |
Module      : Monsters
Description : Definition of all the monsters in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Monsters where

data Monster = Zombie | Ghost

instance Show Monster where
    show Zombie = "🧟"
    show Ghost = "👻"
