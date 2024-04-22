{- |
Module      : World
Description : Building blocks for the game world
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.World where

import Control.Arrow ((>>>))
import Control.Lens (both, makeLenses, over, to, (&), (.~), (^.))
import Creatures.Monsters
import GHC.Arr

type Coordinate = (Int, Int)
type World = [Level]

data Direction = North | East | South | West
data VerticalDirection = Upwards | Downwards
data Cell
    = Door
    | Empty
    | Floor
    | Stair VerticalDirection
    | Tunnel
    | Wall

instance Show Cell where
    show (Stair Downwards) = "V "
    show (Stair Upwards) = "Λ "
    show Door = "λ "
    show Empty = "ε "
    show Floor = ". "
    show Tunnel = ". "
    show Wall = "# "

data Level = Level
    { _cells :: Array Coordinate Cell
    , _monsters :: [Monster]
    }
    deriving (Show)

makeLenses ''Level

-- | Get the width and height of the level
dimensions :: Level -> (Int, Int)
dimensions level = level ^. cells . to (bounds >>> snd >>> over both (+ 1))

-- | Get only the width of the level
width :: Level -> Int
width = snd . dimensions

-- | Get only the height of the level
height :: Level -> Int
height = fst . dimensions

isTraversible :: Cell -> Bool
isTraversible Door = True
isTraversible Empty = False
isTraversible Floor = True
isTraversible (Stair _) = True
isTraversible Tunnel = True
isTraversible Wall = False

-- ===============
-- Constant levels
-- ===============

emptyLevel :: Level
emptyLevel =
    Level
        (listArray ((0, 0), (9, 9)) (repeat Floor) // [((3, 3), Stair Downwards)])
        []

firstLevel :: Level
firstLevel = Level cs ms
  where
    cs =
        listArray
            ((0, 0), (12, 9))
            ( [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                <> [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty, Empty]
                <> [Empty, Door, Floor, Floor, Floor, Floor, Floor, Wall, Empty, Empty]
                <> [Empty, Wall, Floor, Floor, Floor, Floor, Floor, Wall, Empty, Empty]
                <> [Empty, Wall, Wall, Wall, Door, Wall, Wall, Wall, Empty, Empty]
                <> [Empty, Empty, Empty, Empty, Tunnel, Empty, Empty, Empty, Empty, Empty]
                <> [Empty, Empty, Empty, Empty, Tunnel, Empty, Empty, Empty, Empty, Empty]
                <> [Empty, Empty, Empty, Empty, Tunnel, Empty, Empty, Empty, Empty, Empty]
                <> [Empty, Empty, Wall, Wall, Door, Wall, Wall, Wall, Empty, Empty]
                <> [Empty, Empty, Wall, Floor, Floor, Stair Upwards, Floor, Wall, Empty, Empty]
                <> [Empty, Empty, Wall, Floor, Floor, Floor, Floor, Wall, Empty, Empty]
                <> [Empty, Empty, Wall, Wall, Wall, Door, Wall, Wall, Empty, Empty]
                <> [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
            )

    ms = [zombie, zombie & position .~ (2, 4)]
