{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Player where

import World (Coordinate)

data Player = Player
    { name :: String
    , pos :: Coordinate
    }

instance Show Player where
    show _ = "😎"
