module Items.Food where

import Control.Monad.Random 
import Data.Bifunctor (Bifunctor(first))
import Draw 
import Brick (txt)

data Food = Apple | Banana deriving (Show, Bounded, Enum)

instance Random Food where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

instance Drawable Food where
    draw True Apple = txt "A "
    draw False Apple = txt "🍎"
    draw True Banana = txt "B "
    draw False Banana = txt "🍌"
