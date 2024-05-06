module World.Level where

import Control.Lens.TH (makeLenses)
import Creatures.Monsters
import Data.Data (Proxy (..))
import GHC.Arr (Array)
import GHC.TypeLits (KnownNat, Natural, natVal)
import World.Cells

type Coordinate = (Int, Int)
data Direction = North | East | South | West deriving (Show)
type World (cols :: Natural) (rows :: Natural) = [Level cols rows]
data Level (cols :: Natural) (rows :: Natural) = Level
    { _cells :: Array Coordinate Cell
    , _up :: Coordinate
    , _down :: Coordinate
    , _monsters :: [Monster]
    }

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
