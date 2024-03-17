{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Player
Description : Everything that has to do with the player
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Player where

import Control.Lens 
import World (Coordinate)
import Items 

data Player = Player
    { _name :: String
    , _pos :: Coordinate
    , _hand :: Maybe Item 
    , _helmet :: Maybe Item 
    , _armour :: Maybe Item 
    , _gloves :: Maybe Item 
    , _boots :: Maybe Item 
    , _inventory :: [Item]
    }

makeLenses ''Player

instance Show Player where
    show _ = "😎"
