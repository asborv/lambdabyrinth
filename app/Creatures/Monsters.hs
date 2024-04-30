{- |
Module      : Monsters
Description : Definition of all the monsters in the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Monsters where

import Brick (txt)
import Config (Config (difficulty), Difficulty (..))
import Control.Lens ((%~), (^.))
import Control.Lens.Lens ((&))
import Control.Lens.TH (makeLenses)
import Control.Monad.Reader (ReaderT, asks)
import Creatures.Combatant
import qualified Data.Text as T
import Draw

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

instance Drawable Monster where
    draw False monster = txt . T.pack $ show monster
    draw True _ = txt "! "

zombie :: Monster
zombie =
    Monster
        { _health = 32
        , _power = 32
        , _monsterType = Zombie
        , _position = (3, 2)
        }

instance Combatant Monster where
    attack :: (Combatant c, Monad m) => Monster -> c -> ReaderT Config m c
    me `attack` you = do
        d <- asks difficulty
        let modifier = case d of
                Easy -> 0.8 :: Double
                Medium -> 1
                Hard -> 1.5
            damage = round $ fromIntegral (me ^. power) * modifier
        return $ you `acceptDamage` damage

    acceptDamage :: Monster -> Int -> Monster
    acceptDamage me damage = me & health %~ subtract damage
