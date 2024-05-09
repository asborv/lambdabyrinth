{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Items.Chests where

import Items.Armour
import Items.Weapons

data SomeArmour where
    SomeArmour :: Armour s -> SomeArmour

instance Show SomeArmour where
    show (SomeArmour armour) = show armour

data Chest where
    Open :: Chest
    Closed :: Maybe (Either Weapon SomeArmour) -> Chest
