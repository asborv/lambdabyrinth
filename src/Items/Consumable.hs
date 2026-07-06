module Items.Consumable where

import System.Random.Stateful


import Data.Bool (bool)

type Duration = Int

data EffectType = Heal | Damage deriving (Bounded, Enum)
data Potency = Minor | Major | Extreme deriving (Bounded, Enum)

data Effect
    = Instant !Potency !EffectType
    | Gradual !Potency !EffectType !Duration

newtype Consumable = Potion Effect

instance Show Consumable where
    show (Potion (Instant potency effectType))   = "instant potion of " <> show potency <> " " <> show effectType
    show (Potion (Gradual potency effectType _)) = "gradual potion of " <> show potency <> " " <> show effectType

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

instance Uniform Effect where
    uniformM g = uniformM @Bool g >>= bool
        (Instant <$> uniformM g <*> uniformM g)
        (Gradual <$> uniformM g <*> uniformM g <*> uniformRM (2, 5) g)

instance Uniform Consumable where
    uniformM g = Potion <$> uniformM g

power :: Potency -> Int
power Minor = 10
power Major = 20
power Extreme = 30
