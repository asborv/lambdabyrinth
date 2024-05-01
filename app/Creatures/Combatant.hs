{- | 
Module      : Creatures.Combatant
Description : Typeclass for creatures that can attack and take damage
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Combatant where
import Config (Config)
import Control.Monad.Reader (ReaderT)

class Combatant a where
    attack :: (Combatant c, Monad m) => a -> c -> ReaderT Config m c
    takeDamage :: a -> Int -> a
