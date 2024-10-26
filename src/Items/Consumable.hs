module Items.Consumable where

import Control.Monad.Random
import Data.Bifunctor (Bifunctor (first))

type Duration = Int

data EffectType = Heal | Damage deriving (Bounded, Enum)
data Potency = Minor | Major | Extreme deriving (Bounded, Enum)

data Effect
    = Instant Potency EffectType
    | Gradual Potency EffectType Duration

newtype Consumable = Potion Effect

instance Show Potency where
    show Minor = "minor"
    show Major = "major"
    show Extreme = "extreme"

instance Show EffectType where
    show Heal = "healing"
    show Damage = "damage"

instance Show Consumable where
    show (Potion (Instant potency effectType)) = "instant potion of " <> show potency <> " " <> show effectType
    show (Potion (Gradual potency effectType _)) = "gradual potion of " <> show potency <> " " <> show effectType

instance Random EffectType where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

instance Random Potency where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

instance Random Effect where
    random g = case random g of
        (True, g') ->
            let (potency, g'') = random g'
                (effectType, g''') = random g''
             in (Instant potency effectType, g''')
        (False, g') ->
            let (potency, g'') = random g'
                (effectType, g''') = random g''
                (duration, g'''') = randomR (2, 5) g'''
             in (Gradual potency effectType duration, g'''')
    randomR _ = random

instance Random Consumable where
    random g = first Potion $ random g
    randomR _ = random

power :: Potency -> Int
power Minor = 10
power Major = 20
power Extreme = 30
