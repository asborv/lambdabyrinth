{-# LANGUAGE ImportQualifiedPost #-}

module Items.Chests where

import Control.Monad.Random
import Data.Bifunctor (first)
import Data.Kind
import Items.Armour qualified as A
import Items.Food qualified as F
import Items.Weapons qualified as W
import Items.ItemKind

data ChestItem :: ItemKind -> Type where
    Armour :: A.SomeArmour -> ChestItem 'ArmourK
    Weapon :: W.Weapon -> ChestItem 'WeaponK
    Food :: F.Food -> ChestItem 'FoodK

data BoxedChestItem where
    Boxed :: ChestItem a -> BoxedChestItem

data Chest where
    Open :: Chest
    Closed :: Maybe BoxedChestItem -> Chest

instance Show BoxedChestItem where
    show (Boxed (Armour a)) = "Armour: " <> show a
    show (Boxed (Weapon w)) = "Weapon: " <> show w
    show (Boxed (Food f)) = "Food: " <> show f

instance Random BoxedChestItem where
    random g = case random g of
        (ArmourK, g') -> first (Boxed . Weapon) $ random g'
        (WeaponK, g') -> first (Boxed . Armour) $ random g'
        (FoodK, g') -> first (Boxed . Food) $ random g'
    randomR _ = random

instance Random Chest where
    random g =
        let (hasContent, g') = random g
         in if hasContent
                then first (Closed . Just) (random g')
                else (Closed Nothing, g')
    randomR _ = random
