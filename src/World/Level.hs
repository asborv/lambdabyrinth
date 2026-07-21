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
    , Zipper(..)
    , goUp
    , goDown
    , focusIndex
    , fromList
    , cells, visibility, up, down, monsters
    , dimensions
    , width
    , height
    , surrounding
    , transposeCoordinate
    , currentLevel
    ) where

import Control.Lens.TH (makeLenses)
import Data.Data (Proxy (..))
import qualified Data.Map as Map
import GHC.Arr (Array)
import GHC.TypeLits (KnownNat, Natural, natVal)

import Creatures.Monsters
import World.Cells
import Control.Lens (Lens', lens)

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

data Zipper a = Zipper
    { left   :: [a]
    , middle :: a
    , right  :: [a]
    }
    deriving (Show)

goDown :: Zipper a -> Zipper a
goDown z@(Zipper _  _ [])       = z
goDown   (Zipper ls m (r : rs)) = Zipper (m : ls) r rs

goUp :: Zipper a -> Zipper a
goUp z@(Zipper [] _ _)        = z
goUp   (Zipper (l : ls) m rs) = Zipper ls l (m : rs)

focusIndex :: Zipper a -> Int
focusIndex   (Zipper []      _ _) = 0
focusIndex z@(Zipper (_ : _) _ _) = 1 + focusIndex (goUp z)

fromList :: [a] -> Zipper a
fromList []       = error "Can't make zipper from empty list"
fromList (x : xs) = Zipper [] x xs

currentLevel :: Lens' (Zipper (Level cols rows)) (Level cols rows)
currentLevel = lens middle (\z newLevel -> z { middle = newLevel })

type World (cols :: Natural) (rows :: Natural) = Zipper (Level cols rows)

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
