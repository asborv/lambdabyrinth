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
import Control.Monad.Random
import Control.Monad.Reader (ReaderT, asks)
import Creatures.Combatant
import Data.Bifunctor (first)
import qualified Data.Text as T
import Draw
import Scenes.Game.Attributes

data MonsterType = Zombie | Ghost deriving (Show, Eq, Bounded, Enum)

data Monster = Monster
    { _health :: Int
    , _monsterType :: MonsterType
    , _position :: (Int, Int)
    }
    deriving (Eq)

makeLenses ''Monster

power :: Monster -> Int
power monster = case monster ^. monsterType of
    Zombie -> 32
    Ghost -> 25

instance Random MonsterType where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

instance Random Monster where
    random g =
        let (monsterType', g') = random g
            (health', g'') = randomR (40, 100) g'
         in (Monster health' monsterType' (0, 0), g'')
    randomR _ = random

instance Show Monster where
    show monster = case monster ^. monsterType of
        Zombie -> "🧟\b "
        Ghost -> "👻\b "

instance Drawable Monster where
    draw False monster = txt . T.pack $ show monster
    draw True (Monster {_monsterType}) =
        withSymbolAttr MonsterAttr $ case _monsterType of
            Zombie -> txt "Z "
            Ghost -> txt "G "

instance Combatant Monster where
    attack :: (Combatant c, Monad m) => Monster -> c -> ReaderT Config m c
    me `attack` you = do
        d <- asks difficulty
        let modifier = case d of
                Easy -> 0.8 :: Double
                Medium -> 1
                Hard -> 1.5
            damage = round $ fromIntegral (power me) * modifier
        return $ you `takeDamage` damage

    takeDamage :: Monster -> Int -> Monster
    takeDamage me damage = me & health %~ subtract damage
