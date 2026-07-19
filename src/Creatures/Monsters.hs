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
import Draw
import Scenes.Game.Attributes
import System.Random.Stateful

data MonsterType = Zombie | Ghost deriving (Show, Eq, Enum, Bounded)

data Monster = Monster
    { _health :: !Int
    , _monsterType :: !MonsterType
    , _position :: !(Int, Int)
    }
    deriving (Eq)

makeLenses ''Monster

power :: Monster -> Int
power monster = case monster ^. monsterType of
    Zombie -> 32
    Ghost -> 25

instance Uniform MonsterType where
    uniformM = uniformEnumM

instance Uniform Monster where
    uniformM g = Monster
        <$> uniformRM (40, 100) g
        <*> uniformM g
        <*> pure (0, 0)

instance Drawable Monster where
    draw asciiOnly monster =
        let glyph = case monster ^. monsterType of
              Zombie -> Glyph (txt "🧟\b ") (txt "Z ")
              Ghost  -> Glyph (txt "👻\b ") (txt "G ")
         in withSymbolAttr MonsterAttr (draw asciiOnly glyph)

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
