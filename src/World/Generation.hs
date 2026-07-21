{- |
Module      : World.Generation
Description : All about generating levels in the game world
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Generation (generateLevel) where

import Control.Lens.Operators
import Control.Monad.Fix
import Data.Bifoldable
import Data.Data
import Data.Function
import Data.List
import GHC.Arr
import GHC.TypeLits
import System.Random.Stateful

import Creatures.Monsters
import Items.Chest
import Utils (count)
import World.Cells
import World.Level (Coordinate, Level (..), surrounding, transposeCoordinate, Visibility (..))
import World.Tree
import qualified Data.Map as Map

{-# DEPRECATED uniformIO "Use a proper stateful gen instead" #-}
uniformIO :: Uniform a => IO a
uniformIO = uniformM globalStdGen

uniformRIO :: UniformRange a => (a, a) -> IO a
uniformRIO range = uniformRM range globalStdGen

data Direction
    = Vertical
    | Horizontal
    deriving (Show, Enum, Bounded)

instance Uniform Direction where
    uniformM = uniformEnumM

-- | A rectangle that is defined by its upper right and lower right corners
type Rectangle = (Coordinate, Coordinate)

data RectangleSplitSpec = RectangleSplitSpec !Double !Direction

instance Uniform RectangleSplitSpec where
    uniformM g = RectangleSplitSpec
        <$> uniformRM (0.4, 0.6) g
        <*> uniformM g

transposeRect :: Rectangle -> Rectangle
transposeRect (p, q) = (transposeCoordinate q, transposeCoordinate p)

-- | Split a rectangle according to a @RectangleSplitSpec@
-- into a branch of rectangle leaves.
-- Horizontal splitting is done by transposition of vertical splitting
splitRectangle :: Rectangle -> RectangleSplitSpec -> BinaryTree Rectangle
splitRectangle rect (RectangleSplitSpec ratio Horizontal) =
    transposeRect
    <$> splitRectangle
        (transposeRect rect)
        (RectangleSplitSpec ratio Vertical)
splitRectangle (topLeft, bottomRight) (RectangleSplitSpec ratio Vertical) =
    let splitBasis   = fromIntegral $ snd bottomRight - snd topLeft
        offset       = snd topLeft                         -- The offset from which to compute the split point
        splitPoint   = round (ratio * splitBasis) + offset -- Split at some point in range of the split basis
        bottomRight' = (fst bottomRight, splitPoint)       -- The new bounding rectangle corners
        topLeft'     = (fst topLeft, splitPoint)
        left         = (topLeft, bottomRight')             -- The branches to split this leaf into
        right        = (topLeft', bottomRight)
     in Leaf left :-: Leaf right

{- | Split two random leaves in a binary tree into two new leaves
The bisection is done by a random direction and a random point (within a ratio range) along that direction
-}
splitTree :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
splitTree (Leaf x)         = splitRectangle x <$> uniformIO
splitTree (left :-: right) = do
    shouldSplitLeft <- uniformIO @Bool
    if shouldSplitLeft
        then (left :-:) <$> splitTree right
        else splitTree left <&> (:-: right)

-- | Create an array representation of each room in the binarytree's leaves
getRooms :: BinaryTree Rectangle -> [Array Coordinate Cell]
getRooms = map (\rectangle -> listArray rectangle (repeat Floor)) . flatten

{- | Take a room, and push its walls inwards by a random ratio.
 The shrinking is symmetric on the horizontal and vertical axis, respecitvely.
 (The walls will be pushed in by _at least_ 1 cell)
-}
shrinkWalls :: Rectangle -> Double -> Rectangle
shrinkWalls ((y0, x0), (y1, x1)) ratio = do
    let width = fromIntegral (x1 - x0) :: Double
        height = fromIntegral (y1 - y0) :: Double
        dx = max 1 (round (ratio * width))
        dy = max 1 (round (ratio * height))
     in ((y0 + dy, x0 + dx), (y1 - dy, x1 - dx))

-- | Calculate the center of a rectangle
center :: Rectangle -> Coordinate
center ((y0, x0), (y1, x1)) = ((y0 + y1) `div` 2, (x0 + x1) `div` 2)

-- | Dig an L-shaped tunnel between two nodes
dig :: Edge -> [(Coordinate, Cell)]
dig ((y0, x0), (y1, x1)) = map (,Tunnel) $ vertical <> horizontal
  where
    vertical = [(y, x0) | y <- [min y0 y1 .. max y0 y1]]
    horizontal = [(y1, x) | x <- [min x0 x1 .. max x0 x1]]

-- | Given a ratio, and a list of coordinates, generate random items at some of the coordinates
generateByRatioFromPositions :: Uniform a => Double -> [Coordinate] -> IO [(Coordinate, a)]
generateByRatioFromPositions ratio cells = do
    rs <- mapM (const (uniformRIO @Double (0.0, 1.0))) cells
    let cellsToPopulate = map fst $ filter ((< ratio) . snd) (zip cells rs)
    items <- mapM (const uniformIO) cellsToPopulate
    return $ zip cellsToPopulate items

{- | Generate staircases, one up, one down.
They are placed at dead ends (leaves) of the provided edges, as distant as possible away from each other.
-}
generateStairs :: [Edge] -> (Coordinate, Coordinate)
generateStairs edges = longest
  where
    nodes = concatMap biList edges
    deadEnds = filter (\node -> count node nodes == 1) nodes
    longest =
        maximumBy
            (compare `on` weight)
            [(a, b) | a <- deadEnds, b <- deadEnds]

buildLevelFromTree ::
    forall cols rows. (KnownNat cols, KnownNat rows) =>
    BinaryTree Rectangle ->
    IO (Level cols rows)
buildLevelFromTree tree = do
    tree' <- traverse shrinkWalls tree <$> uniformRIO (0.3, 0.4)
    let rooms             = getRooms tree'                             -- Get the rooms of the (shrunken) binary tree
        centers           = center <$> flatten tree'                   -- Get the center of each room
        connectedRooms    = mst centers                                -- Use the rooms' centers to calculate an MST between them
        tunnels           = concatMap dig connectedRooms               -- Connect rooms by tunnels by the MST
        (up, down)        = generateStairs connectedRooms              -- Place the up- and downwards staircases as long away from each other as possible
        floorCells        = concatMap indices rooms                    -- Positions where items and monsters can be placed
        width             = fromInteger $ natVal (Proxy @cols) - 1
        height            = fromInteger $ natVal (Proxy @rows) - 1
        boundingRectangle = ((0, 0), (height, width))
        allWalls          = listArray boundingRectangle (repeat Wall)  -- Level's dimensions filled with walls

    -- Placing chests and monsters on floor (not on stairs)
    monstersAndPositions <- generateByRatioFromPositions @Monster 0.05 (floorCells \\ [up, down])
    chests <- generateByRatioFromPositions @Chest 0.005 (floorCells \\ [up, down])
    let chests' = map (Chest <$>) chests
        monsters = Map.fromList monstersAndPositions

        -- Order matters since we "paint over"
        cellsToPaint = concatMap assocs rooms                  -- Rooms to be carved
            <> chests'                                         -- Chests to be placed
            <> tunnels                                         -- Tunnels between rooms
            <> [(up, Stair Upwards), (down, Stair Downwards)]  -- Stairs up and down

        visibleCells = map (, Visible) (surrounding up)

    return $ Level
        (allWalls // cellsToPaint)
        (listArray boundingRectangle (repeat Unseen) // visibleCells)
        up
        down
        monsters

generateLevel ::
    forall cols rows.
    (KnownNat cols, KnownNat rows) =>
    IO (Level cols rows)
generateLevel =
    let width = fromInteger $ natVal (Proxy @cols) - 1
        height = fromInteger $ natVal (Proxy @rows) - 1
        boundingRectangle = ((0, 0), (height, width))
        initial = Leaf boundingRectangle
     in flip fix initial $ \loop tree -> do
            tree' <- splitTree tree
            if leaves tree' <= 5
                then loop tree'
                else buildLevelFromTree tree'
