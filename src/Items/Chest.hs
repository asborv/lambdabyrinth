module Items.Chest where

import Brick (txt)
import System.Random.Stateful

import Draw
import Items.Item
import Scenes.Game.Attributes

data Chest where
    Open :: Chest
    Closed :: Maybe BoxedItem -> Chest

instance Uniform Chest where
    uniformM g = do
        hasContent <- uniformM @Bool g
        if hasContent
            then return (Closed Nothing)
            else Closed . Just <$> uniformM g

instance Drawable Chest where
    draw False Open       = txt "📭\b "
    draw False (Closed _) = txt "📫\b "
    draw True  Open       = withSymbolAttr ChestAttr $ txt "()"
    draw True  (Closed _) = withSymbolAttr ChestAttr $ txt "[]"
