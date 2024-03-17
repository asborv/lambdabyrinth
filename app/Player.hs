{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Player where

import Control.Lens (makeLenses)
import World (Coordinate)

data Player = Player
    { _name :: String
    , _pos :: Coordinate
    }

instance Show Player where
    show _ = "😎"

makeLenses ''Player
