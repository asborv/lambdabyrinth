{- |
Module      : Monsters
Description : Definition of all the monsters in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Monsters where

import Control.Lens ((%~), (^.))
import Control.Lens.Lens ((&))
import Control.Lens.TH (makeLenses)
import Creatures.Combatant

data MonsterType = Zombie | Ghost deriving (Show, Eq)
data Monster = Monster
    { _health :: Int
    , _monsterType :: MonsterType
    , _power :: Int
    , _position :: (Int, Int)
    }
    deriving (Eq)

makeLenses ''Monster

instance Show Monster where
    show monster = case monster ^. monsterType of
        Zombie -> "🧟\b "
        Ghost -> "👻\b "

zombie :: Monster
zombie =
    Monster
        { _health = 32
        , _power = 32
        , _monsterType = Zombie
        , _position = (3, 2)
        }

instance Combatant Monster where
    attack :: Combatant c => Monster -> c -> c
    me `attack` you = you `acceptDamage` (me ^. power)

    acceptDamage :: Monster -> Int -> Monster
    acceptDamage me damage = me & health %~ subtract damage
