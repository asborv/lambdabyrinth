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
import qualified Data.Text as T
import Draw

data MonsterType = Zombie | Ghost deriving (Show, Eq)
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
    random g = case randomR (True, False) g of
        (True, g') -> (Zombie, g')
        (False, g') -> (Ghost, g')
    randomR _ = random

instance Random Monster where
    random g =
        let (mt, g') = random g
            (h, g'') = randomR (40, 100) g'
         in (Monster h mt (0, 0), g'')
    randomR _ = random

instance Show Monster where
    show monster = case monster ^. monsterType of
        Zombie -> "🧟\b "
        Ghost -> "👻\b "

instance Drawable Monster where
    draw False monster = txt . T.pack $ show monster
    draw True (Monster {_monsterType}) = case _monsterType of
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