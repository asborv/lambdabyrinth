module Items.Item where

import           Data.Kind (Type)
import           System.Random.Stateful
import qualified Items.Armour as A
import qualified Items.Consumable as C
import qualified Items.Weapon as W

data ItemKind = ArmourK | WeaponK | ConsumableK deriving (Bounded, Enum)

instance Uniform ItemKind where
    uniformM = uniformEnumM

data Item :: ItemKind -> Type where
    Armour :: A.BoxedArmour -> Item 'ArmourK
    Weapon :: W.Weapon -> Item 'WeaponK
    Consumable :: C.Consumable -> Item 'ConsumableK

data BoxedItem where
    Boxed :: Item a -> BoxedItem

instance Show BoxedItem where
    show (Boxed (Armour a)) = show a
    show (Boxed (Weapon w)) = show w
    show (Boxed (Consumable c)) = show c

instance Uniform BoxedItem where
    uniformM g = uniformM @ItemKind g >>= \case
        ArmourK -> Boxed . Armour <$> uniformM g
        WeaponK -> Boxed . Weapon <$> uniformM g
        ConsumableK -> Boxed . Consumable <$> uniformM g
