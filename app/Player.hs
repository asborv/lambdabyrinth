{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Player where

import World (Coordinate)

data Player = Player
    { _name :: String
    , _pos :: Coordinate
    }

instance Show Player where
    show _ = "😎"
