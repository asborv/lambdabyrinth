module Creatures.Combatant where
import Config (Config)
import Control.Monad.Reader (ReaderT)

class Combatant a where
    attack :: (Combatant c, Monad m) => a -> c -> ReaderT Config m c
    acceptDamage :: a -> Int -> a
