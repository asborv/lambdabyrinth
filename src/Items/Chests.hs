{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Items.Chests where

import Control.Arrow (first)
import Control.Monad.Random
import Items.Armour
import Items.Weapons

data SomeArmour where
    SomeArmour :: Armour s -> SomeArmour

instance Random SomeArmour where
    random g = case randomR @Int (0, 3) g of
        (0, g') -> first (SomeArmour . Helmet) (random g')
        (1, g') -> first (SomeArmour . Cuirass) (random g')
        (2, g') -> first (SomeArmour . Gloves) (random g')
        (3, g') -> first (SomeArmour . Boots) (random g')
        _ -> error "Impossible"
    randomR _ = random

instance Show SomeArmour where
    show (SomeArmour armour) = show armour

data Chest where
    Open :: Chest
    Closed :: Maybe (Either Weapon SomeArmour) -> Chest

instance Random Chest where
    random g =
        let (isWeapon, g') = random @Bool g
            (weapon, g'') = random g'
            (armour, g''') = random g''
         in (Closed . Just $ if isWeapon then Left weapon else Right armour, g''')
    randomR _ = random
