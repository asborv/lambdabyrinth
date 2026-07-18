module Items.Consumable
    ( EffectType(..)
    , Potency(..)
    , Consumable(..)
    , Duration
    , Immediacy(..)
    , Effect(..)
    , power
    ) where

import Data.Bool (bool)
import System.Random.Stateful

type Duration = Int

data EffectType = Heal | Damage deriving (Bounded, Enum)
data Potency = Minor | Major | Extreme deriving (Bounded, Enum)
data Immediacy = Instant | Gradual !Duration

data Effect = Effect !Immediacy !Potency !EffectType

newtype Consumable = Potion Effect

instance Show Immediacy where
    show Instant     = "instant"
    show (Gradual _) = "gradual"

instance Show Consumable where
    show (Potion (Effect immediacy potency effectType)) = show immediacy
        <> show potency
        <> " "
        <> show effectType

instance Show Potency where
    show Minor = "minor"
    show Major = "major"
    show Extreme = "extreme"

instance Show EffectType where
    show Heal = "healing"
    show Damage = "damage"

instance Uniform EffectType where
    uniformM = uniformEnumM

instance Uniform Potency where
    uniformM = uniformEnumM

instance Uniform Immediacy where
    uniformM g = uniformM @Bool g >>= bool
        (pure Instant)
        (Gradual <$> uniformRM (2, 5) g)

instance Uniform Effect where
    uniformM g = Effect
        <$> uniformM g
        <*> uniformM g
        <*> uniformM g

instance Uniform Consumable where
    uniformM g = Potion <$> uniformM g

power :: Potency -> Int
power Minor = 10
power Major = 20
power Extreme = 30
