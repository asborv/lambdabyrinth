{- |
Module      : Items.Material
Description : All materials and their stats that items can be made of
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Material (Material (..), materialBonus) where

import Brick (txt)
import Draw
import System.Random.Stateful

data Material = Stone | Wood | Diamond
    deriving (Show, Enum, Bounded)

instance Drawable Material where
    draw False Stone   = txt "🪨\b "
    draw False Wood    = txt "🪵\b "
    draw False Diamond = txt "💎\b "
    draw True Stone    = txt "S "
    draw True Wood     = txt "W "
    draw True Diamond  = txt "D "

instance Uniform Material where
    uniformM = uniformEnumM

materialBonus :: Material -> Int
materialBonus Wood    = 5
materialBonus Stone   = 15
materialBonus Diamond = 30
