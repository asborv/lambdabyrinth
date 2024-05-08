{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Player where

import Brick (txt)
import Config (Config (difficulty), Difficulty (..))
import Control.Lens (makeLenses, (%~), (&), (^.))
import Control.Lens.Combinators (to)
import Control.Monad.Reader (ReaderT, asks)
import Creatures.Combatant
import Draw
import Items.Armour
import Items.Weapons
import World.Level
import qualified Data.Text as T

data Class = Wizard | Warrior | Rogue deriving (Show, Eq)

data Player = Player
    { _name :: T.Text
    , _pos :: Coordinate
    , _hand :: Maybe Weapon
    , _helmet :: Maybe Armour
    , _cuirass :: Maybe Armour
    , _gloves :: Maybe Armour
    , _boots :: Maybe Armour
    , _health :: Int
    , _characterClass :: Class
    }

makeLenses ''Player

instance Drawable Player where
    draw asciiOnly player = txt symbol
      where
        symbol = if asciiOnly then "P " else case player ^. characterClass of
            Warrior -> "⚔️\b "
            Rogue -> "🦹\b "
            Wizard -> "🧙\b "

instance Combatant Player where
    attack :: (Combatant c, Monad m) => Player -> c -> ReaderT Config m c
    me `attack` you = do
        d <- asks difficulty

        let classDamage = me ^. characterClass & classPower
            weaponDamage = me ^. hand . to (maybe 0 power)
            damage = round $ fromIntegral (classDamage + weaponDamage) * modifier
            modifier = case d of
                Easy -> 1.5 :: Double
                Medium -> 1
                Hard -> 0.8

        return $ you `takeDamage` damage

    takeDamage :: Player -> Int -> Player
    takeDamage me damage = me & health %~ subtract damage

classPower :: Class -> Int
classPower = \case
    Wizard -> 15
    Warrior -> 85
    Rogue -> 45
