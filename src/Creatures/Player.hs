{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Creatures.Player where

import Config (Config (difficulty), Difficulty (..))
import Control.Lens (makeLenses, (%~), (&), (?~), (^.), (+~), (-~), (.~))
import Control.Lens.Combinators (to)
import Control.Monad.Reader (ReaderT, asks)
import Creatures.Combatant
import qualified Data.Text as T
import qualified Items.Armour as A
import qualified Items.Weapon as W
import World.Level
import Draw
import Brick (txt)
import Items.Item
import qualified Items.Consumable as C
import Items.Consumable (Effect(Gradual))

data Class = Wizard | Warrior | Rogue deriving (Show, Eq)

data Player = Player
    { _name :: T.Text
    , _pos :: Coordinate
    , _hand :: Maybe W.Weapon
    , _helmet :: Maybe (A.Armour 'A.Head)
    , _cuirass :: Maybe (A.Armour 'A.Body)
    , _gloves :: Maybe (A.Armour 'A.Hands)
    , _boots :: Maybe (A.Armour 'A.Feet)
    , _health :: Int
    , _maxHealth :: Int
    , _characterClass :: Class
    , _effects :: [(C.Potency, C.EffectType, C.Duration)]
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
            weaponDamage = me ^. hand . to (maybe 0 W.power)
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

consume :: Player -> C.Consumable -> Player
consume me (C.Potion e@(C.Instant _ _)) = applyEffect e me
consume me (C.Potion (C.Gradual p e t)) = me & effects %~ ((p, e, t) :)

applyActiveEffects :: Player -> Player
applyActiveEffects me@Player {_effects = es} =
    let me' =
            foldr
                (applyEffect . \(potency, effectType, duration) -> Gradual potency effectType duration)
                me
                es
        es' =
            filter (\(_, _, duration) -> duration > 0)
                . map (\(potency, effectType, duration) -> (potency, effectType, duration - 1))
                $ es
     in me' & effects .~ es'

applyEffect :: C.Effect -> Player -> Player
applyEffect (C.Instant potency C.Heal) me = me & health .~ min (C.power potency) (me ^. maxHealth)
applyEffect (C.Instant potency C.Damage) me = me & health -~ C.power potency
applyEffect (C.Gradual potency C.Heal _) me = me & health +~ min (C.power potency) (me ^. maxHealth)
applyEffect (C.Gradual potency C.Damage _) me = me & health -~ C.power potency

pickup :: BoxedItem -> Player -> Player
pickup (Boxed (Consumable consumable)) me = me `consume` consumable
pickup (Boxed (Weapon weapon)) me =
    if me `shouldEquip` Left weapon
        then me & hand ?~ weapon
        else me
pickup (Boxed (Armour (A.Boxed armour))) me =
    if me `shouldEquip` Right armour
        then case armour of
            (A.Helmet _) -> me & helmet ?~ armour
            (A.Cuirass _) -> me & cuirass ?~ armour
            (A.Gloves _) -> me & gloves ?~ armour
            (A.Boots _) -> me & boots ?~ armour
        else me

shouldEquip :: Player -> Either W.Weapon (A.Armour s) -> Bool
shouldEquip me (Left weapon) = all (\w -> W.power weapon > W.power w) (me ^. hand)
shouldEquip me (Right armour@(A.Helmet _)) = all (\a -> A.defence armour > A.defence a) (me ^. helmet)
shouldEquip me (Right armour@(A.Cuirass _)) = all (\a -> A.defence armour > A.defence a) (me ^. cuirass)
shouldEquip me (Right armour@(A.Gloves _)) = all (\a -> A.defence armour > A.defence a) (me ^. gloves)
shouldEquip me (Right armour@(A.Boots _)) = all (\a -> A.defence armour > A.defence a) (me ^. boots)
