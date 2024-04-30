{- |
Module      : Items.Armour
Description : Armour, implementations, and their stats
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Items.Armour (Armour (..), defence) where

import Brick (Widget, (<+>))
import Brick.Widgets.Core (txt)
import Draw
import Items.Materials

data Piece = Helmet | Cuirass | Gloves | Boots
data Armour = Armour {piece :: Piece, material :: Material}

instance Drawable Armour where
    draw :: Bool -> Armour -> Widget n
    draw asciiOnly armour = draw asciiOnly (material armour) <+> txt symbol
      where
        symbol = case (asciiOnly, piece armour) of
            (False, Cuirass) -> "🛡️ "
            (True, Cuirass) -> "# "
            (False, Helmet) -> "🪖 "
            (True, Helmet) -> "^ "
            (False, Gloves) -> "🧤 "
            (True, Gloves) -> "''"
            (False, Boots) -> "👢 "
            (True, Boots) -> ",,"

defence :: Armour -> Int
defence armour = materialBonus (material armour) * pieceBonus
  where
    pieceBonus = case piece armour of
        Helmet -> 10
        Cuirass -> 8
        Gloves -> 24
        Boots -> 12
