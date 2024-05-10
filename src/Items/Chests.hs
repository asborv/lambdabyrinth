{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Items.Chests where

import Control.Monad.Random
import Items.Armour
import Items.Weapons

data Chest where
    Open :: Chest
    Closed :: Maybe (Either Weapon SomeArmour) -> Chest

instance Random Chest where
    random g =
        let (isWeapon, g') = random g
            (weapon, g'') = random g'
            (armour, g''') = random g''
            (hasContent, g'''') = random g'''
         in case (hasContent, isWeapon) of
                (True, True) -> (Closed . Just $ Left weapon, g'''')
                (True, False) -> (Closed . Just $ Right armour, g'''')
                (False, _) -> (Closed Nothing, g'''')
    randomR _ = random
