{- |
Module      : Items.Material
Description : All materials and their stats that items can be made of
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Material (Material (..), materialBonus) where

import Brick (txt)
import Draw
import System.Random
import Data.Bifunctor (first)

data Material = Stone | Wood | Diamond deriving (Show, Enum, Bounded)

instance Drawable Material where
    draw False Stone = txt "🪨\b "
    draw False Wood = txt "🪵\b "
    draw False Diamond = txt "💎\b "
    draw True Stone = txt "S "
    draw True Wood = txt "W "
    draw True Diamond = txt "D "

instance Random Material where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

materialBonus :: Material -> Int
materialBonus = \case
    Wood -> 5
    Stone -> 15
    Diamond -> 30
