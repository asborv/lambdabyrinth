{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : World
Description : Building blocks for the game world
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World where

import Control.Arrow ((>>>))
import Control.Lens (both, makeLenses, over, to, (^.))
import qualified Data.Map as Map
import GHC.Arr
import Monsters

type Coordinate = (Int, Int)
type World = [Level]

data VerticalDirection = Upwards | Downwards
data Cell = Floor | Wall | Door | Stair VerticalDirection

instance Show Cell where
    show Floor = ".."
    show Wall = "##"
    show Door = "λ "
    show (Stair Upwards) = "Λ "
    show (Stair Downwards) = "V "

data Level = Level
    { _cells :: Array Coordinate Cell
    , _monsters :: Map.Map Coordinate Monster
    }
    deriving (Show)

makeLenses ''Level

-- | Get the width and height of the level
dimensions :: Level -> (Int, Int)
dimensions level = level ^. cells . to (bounds >>> snd >>> over both (+ 1))

-- | Get only the width of the level
width :: Level -> Int
width = fst . dimensions

-- | Get only the height of the level
height :: Level -> Int
height = snd . dimensions

-- ===============
-- Constant levels
-- ===============

emptyLevel :: Level
emptyLevel = Level (listArray ((0, 0), (9, 9)) (repeat Floor)) Map.empty

firstLevel :: Level
firstLevel = Level cs ms
  where
    cs = listArray ((0, 0), (19, 19)) (repeat Floor)
    ms = Map.empty
