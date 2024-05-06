module World.Level where

import Control.Lens (both, over, to, (^.))
import Control.Lens.TH (makeLenses)
import Creatures.Monsters
import GHC.Arr (Array, bounds)
import World.Cells

type Coordinate = (Int, Int)
data Direction = North | East | South | West deriving (Show)
type World = [Level]
data Level = Level
    { _cells :: Array Coordinate Cell
    , _up :: Coordinate
    , _down :: Coordinate
    , _monsters :: [Monster]
    }
    deriving (Show)

makeLenses ''Level

-- | Get the width and height of the level
dimensions :: Level -> (Int, Int)
dimensions level = level ^. cells . to (over both (+ 1) . snd . bounds)

-- | Get only the width of the level
width :: Level -> Int
width = snd . dimensions

-- | Get only the height of the level
height :: Level -> Int
height = fst . dimensions
