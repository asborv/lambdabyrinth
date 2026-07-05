{- |
Module      : Items.Armour
Description : Armour, implementations, and their stats
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Armour (Armour (..), defence, Slot (..), BoxedArmour (..)) where

import Brick (Widget, (<+>))
import Brick.Widgets.Core (txt)

import Data.Kind (Type)
import Draw
import Items.Material
import System.Random.Stateful

-- |  The slot an armour piece can be equipped in
data Slot = Head | Body | Hands | Feet deriving (Show, Bounded, Enum)

instance Uniform Slot where
    uniformM = uniformEnumM

{- | Armour for head, body, hands, and feet
The GADT ensures that the type of the armour corresponds to the slot it can be equipped in
-}
data Armour :: Slot -> Type where
    Helmet :: Material -> Armour 'Head
    Cuirass :: Material -> Armour 'Body
    Gloves :: Material -> Armour 'Hands
    Boots :: Material -> Armour 'Feet

data BoxedArmour where
    Boxed :: Armour s -> BoxedArmour

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

instance Uniform BoxedArmour where
    uniformM g = uniformM @Slot g >>= \case
        Head -> Boxed . Helmet <$> uniformM g
        Body -> Boxed . Cuirass <$> uniformM g
        Hands -> Boxed . Gloves <$> uniformM g
        Feet -> Boxed . Helmet <$> uniformM g

instance Show BoxedArmour where
    show (Boxed armour) = show armour
