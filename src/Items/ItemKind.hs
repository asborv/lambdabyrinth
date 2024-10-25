module Items.ItemKind where

import Control.Monad.Random
import Data.Bifunctor (first)

data ItemKind = ArmourK | WeaponK | FoodK deriving (Bounded, Enum)

instance Random ItemKind where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)
