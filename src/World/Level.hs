{- |
Module      : World.Level
Description : Definition of the game world's levels, and util functions
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Level
    ( Coordinate
    , Direction(..)
    , World
    , Visibility(..)
    , Level(..)
    , cells, visibility, up, down, monsters
    , dimensions
    , width
    , height
    , surrounding
    , transposeCoordinate
    ) where

import Control.Lens.TH (makeLenses)
import Data.Data (Proxy (..))
import qualified Data.Map as Map
import GHC.Arr (Array)
import GHC.TypeLits (KnownNat, Natural, natVal)

import Creatures.Monsters
import World.Cells

type Coordinate = (Int, Int)

data Direction = North | East | South | West deriving (Show)

data Visibility = Unseen | Remembered | Visible
    deriving (Eq)

data Level (cols :: Natural) (rows :: Natural) = Level
    { _cells      :: !(Array Coordinate Cell)
    , _visibility :: !(Array Coordinate Visibility)
    , _up         :: !Coordinate
    , _down       :: !Coordinate
    , _monsters   :: !(Map.Map Coordinate Monster)
    }

type World (cols :: Natural) (rows :: Natural) = [Level cols rows]

makeLenses ''Level

dimensions ::
    forall cols rows.
    (KnownNat cols, KnownNat rows) =>
    Level cols rows
    -> (Int, Int)
dimensions _ = (h, w)
  where
    w = fromInteger $ natVal (Proxy @cols)
    h = fromInteger $ natVal (Proxy @rows)

-- | Get only the width of the level
width ::
    forall cols rows.
    (KnownNat cols, KnownNat rows) =>
    Level cols rows
    -> Int
width = snd . dimensions

-- | Get only the height of the level
height ::
    forall cols rows.
    (KnownNat cols, KnownNat rows) =>
    Level cols rows
    -> Int
height = fst . dimensions

surrounding :: Coordinate -> [Coordinate]
surrounding (y, x) =
  [ (y - 1, x - 1), (y - 1, x), (y - 1, x + 1)
  , (y, x - 1)    , (y, x)    , (y, x + 1)
  , (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)
  ]

transposeCoordinate :: Coordinate -> Coordinate
transposeCoordinate (y, x) = (x, y)
