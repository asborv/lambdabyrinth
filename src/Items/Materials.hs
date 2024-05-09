{- |
Module      : Items.Materials
Description : All materials and their stats that items can be made of
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Materials (Material (..), materialBonus) where

import Brick (txt)
import Draw
import System.Random

data Material = Stone | Wood | Diamond deriving (Show)

instance Drawable Material where
    draw False Stone = txt "🪨\b "
    draw False Wood = txt "🪵\b "
    draw False Diamond = txt "💎\b "
    draw True Stone = txt "S "
    draw True Wood = txt "W "
    draw True Diamond = txt "D "

instance Random Material where
    random g =
        let (material, g') = randomR @Int (0, 2) g
         in case material of
                0 -> (Stone, g')
                1 -> (Wood, g')
                2 -> (Diamond, g')
                _ -> error "Impossible"
    randomR _ = random

materialBonus :: Material -> Int
materialBonus = \case
    Wood -> 5
    Stone -> 15
    Diamond -> 30
