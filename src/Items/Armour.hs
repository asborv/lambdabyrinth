{- |
Module      : Items.Armour
Description : Armour, implementations, and their stats
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Armour (Armour (..), defence, Slot (..), SomeArmour(..)) where

import Brick (Widget, (<+>))
import Brick.Widgets.Core (txt)
import Control.Arrow (Arrow (first))
import Data.Kind (Type)
import Draw
import Items.Materials
import System.Random

-- |  The slot an armour piece can be equipped in
data Slot = Head | Body | Hands | Feet deriving (Show, Bounded, Enum)

instance Random Slot where
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)
    random = randomR (minBound, maxBound)

{- | Armour for head, body, hands, and feet
The GADT ensures that the type of the armour corresponds to the slot it can be equipped in
-}
data Armour :: Slot -> Type where
    Helmet :: Material -> Armour 'Head
    Cuirass :: Material -> Armour 'Body
    Gloves :: Material -> Armour 'Hands
    Boots :: Material -> Armour 'Feet

instance Show (Armour a) where
    show (Helmet material) = show material <> " Helmet"
    show (Cuirass material) = show material <> " Cuirass"
    show (Gloves material) = show material <> " Gloves"
    show (Boots material) = show material <> " Boots"

instance Drawable (Armour a) where
    draw :: Bool -> Armour a -> Widget n
    draw asciiOnly armour = case (asciiOnly, armour) of
        (False, Cuirass m) -> draw asciiOnly m <+> txt "🛡️\b "
        (True, Cuirass m) -> draw asciiOnly m <+> txt "# "
        (False, Helmet m) -> draw asciiOnly m <+> txt "🪖\b "
        (True, Helmet m) -> draw asciiOnly m <+> txt "^ "
        (False, Gloves m) -> draw asciiOnly m <+> txt "🧤\b "
        (True, Gloves m) -> draw asciiOnly m <+> txt "''"
        (False, Boots m) -> draw asciiOnly m <+> txt "👢\b "
        (True, Boots m) -> draw asciiOnly m <+> txt ",,"

instance Random (Armour 'Head) where
    random g = first Helmet (random g)
    randomR _ = random

instance Random (Armour 'Body) where
    random g = first Cuirass (random g)
    randomR _ = random

instance Random (Armour 'Hands) where
    random g = first Gloves (random g)
    randomR _ = random

instance Random (Armour 'Feet) where
    random g = first Boots (random g)
    randomR _ = random

-- | Calculate the defence power of an armour piece
defence :: Armour a -> Int
defence (Helmet material) = materialBonus material * slotBonus Head
defence (Cuirass material) = materialBonus material * slotBonus Body
defence (Gloves material) = materialBonus material * slotBonus Hands
defence (Boots material) = materialBonus material * slotBonus Feet

-- | Calculate the amount of defence each piece of armour gives
slotBonus :: Slot -> Int
slotBonus Head = 10
slotBonus Body = 8
slotBonus Hands = 6
slotBonus Feet = 4

data SomeArmour where
    SomeArmour :: Armour s -> SomeArmour

instance Random SomeArmour where
    random g =
        let (material, g') = random g
         in case random g' of
                (Head, g'') -> (SomeArmour $ Helmet material, g'')
                (Body, g'') -> (SomeArmour $ Cuirass material, g'')
                (Hands, g'') -> (SomeArmour $ Gloves material, g'')
                (Feet, g'') -> (SomeArmour $ Boots material, g'')
    randomR _ = random

instance Show SomeArmour where
    show (SomeArmour armour) = show armour
