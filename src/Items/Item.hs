{-# LANGUAGE ImportQualifiedPost #-}

module Items.Item where

import Control.Monad.Random
import Data.Bifunctor (first)
import Data.Kind (Type)
import Items.Armour qualified as A
import Items.Food qualified as F
import Items.Weapons qualified as W

data ItemKind = ArmourK | WeaponK | FoodK deriving (Bounded, Enum)

data BoxedItem where
    Boxed :: Item a -> BoxedItem

data Item :: ItemKind -> Type where
    Armour :: A.SomeArmour -> Item 'ArmourK
    Weapon :: W.Weapon -> Item 'WeaponK
    Food :: F.Food -> Item 'FoodK

instance Random ItemKind where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

instance Show BoxedItem where
    show (Boxed (Armour a)) = "Armour: " <> show a
    show (Boxed (Weapon w)) = "Weapon: " <> show w
    show (Boxed (Food f)) = "Food: " <> show f

instance Random BoxedItem where
    random g = case random g of
        (ArmourK, g') -> first (Boxed . Weapon) $ random g'
        (WeaponK, g') -> first (Boxed . Armour) $ random g'
        (FoodK, g') -> first (Boxed . Food) $ random g'
    randomR _ = random
