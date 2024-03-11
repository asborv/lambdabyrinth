{- |
Module      : World
Description : Building blocks for the game world
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World where

import qualified Data.Map as Map
import GHC.Arr
import Monsters

type World = [Level]
type Level = [Room]
type Coordinate = (Int, Int)

data VerticalDirection = Upwards | Downwards

data Cell = Floor | Wall | Door | Stair VerticalDirection

data Room = Room
    { _cells :: Array Coordinate Cell
    , _monsters :: Map.Map Coordinate Monster
    }
    deriving (Show)

instance Show Cell where
    show Floor = ".."
    show Wall = "##"
    show Door = "λ "
    show (Stair Upwards) = "Λ "
    show (Stair Downwards) = "V "

emptyRoom :: Room
emptyRoom =
    Room
        (listArray ((0, 0), (9, 9)) (replicate 100 Floor))
        (Map.fromList [((2, 2), Zombie), ((4, 5), Ghost)])
