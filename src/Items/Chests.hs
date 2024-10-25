module Items.Chests where

import Control.Monad.Random
import Data.Bifunctor (first)
import Items.Item

data Chest where
    Open :: Chest
    Closed :: Maybe BoxedItem -> Chest

instance Random Chest where
    random g =
        let (hasContent, g') = random g
         in if hasContent
                then first (Closed . Just) (random g')
                else (Closed Nothing, g')
    randomR _ = random
