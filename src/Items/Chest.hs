module Items.Chest where

import Items.Item
import System.Random.Stateful

data Chest where
    Open :: Chest
    Closed :: Maybe BoxedItem -> Chest

instance Uniform Chest where
    uniformM g = do
        hasContent <- uniformM @Bool g
        if hasContent
            then return (Closed Nothing)
            else Closed . Just <$> uniformM g
