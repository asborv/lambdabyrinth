{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Player where

import Config (Config (difficulty), Difficulty (..))
import Control.Lens (makeLenses, (%~), (&), (?~), (^.))
import Control.Lens.Combinators (to)
import Control.Monad.Reader (ReaderT, asks)
import Creatures.Combatant
import qualified Data.Text as T
import Items.Armour
import Items.Weapons
import World.Level
import Draw
import Brick (txt)

data Class = Wizard | Warrior | Rogue deriving (Show, Eq)

data Player = Player
    { _name :: T.Text
    , _pos :: Coordinate
    , _hand :: Maybe Weapon
    , _helmet :: Maybe (Armour 'Head)
    , _cuirass :: Maybe (Armour 'Body)
    , _gloves :: Maybe (Armour 'Hands)
    , _boots :: Maybe (Armour 'Feet)
    , _health :: Int
    , _maxHealth :: Int
    , _characterClass :: Class
    }

makeLenses ''Player

instance Drawable Player where
    draw asciiOnly player = txt symbol
      where
        symbol = if asciiOnly then "P " else case player ^. characterClass of
            Warrior -> "⚔ "
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

equip :: Either Weapon SomeArmour -> Player -> Player
equip (Left weapon) me = me & hand ?~ weapon
equip (Right (SomeArmour armour)) me = case armour of
    (Helmet _) -> me & helmet ?~ armour
    (Cuirass _) -> me & cuirass ?~ armour
    (Gloves _) -> me & gloves ?~ armour
    (Boots _) -> me & boots ?~ armour

shouldEquip :: Either Weapon SomeArmour -> Player -> Bool
shouldEquip (Left weapon) me = all (\w -> power weapon > power w) (me ^. hand)
shouldEquip (Right (SomeArmour armour@(Helmet _))) me = all (\a -> defence armour > defence a) (me ^. helmet)
shouldEquip (Right (SomeArmour armour@(Cuirass _))) me = all (\a -> defence armour > defence a) (me ^. cuirass)
shouldEquip (Right (SomeArmour armour@(Gloves _))) me = all (\a -> defence armour > defence a) (me ^. gloves)
shouldEquip (Right (SomeArmour armour@(Boots _))) me = all (\a -> defence armour > defence a) (me ^. boots)
