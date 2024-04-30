{- |
Module      : Items.Materials
Description : All materials and their stats that items can be made of
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Materials (Material (..), materialBonus) where

import Brick (txt)
import Draw

data Material = Stone | Wood | Diamond

instance Drawable Material where
    draw True Stone = txt "🪨 \b"
    draw True Wood = txt "🪵 \b"
    draw True Diamond = txt "💎 \b"
    draw False Stone = txt "S "
    draw False Wood = txt "W "
    draw False Diamond = txt "D "

materialBonus :: Material -> Int
materialBonus = \case
    Wood -> 5
    Stone -> 15
    Diamond -> 30
