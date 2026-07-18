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

generateLevel ::
    forall cols rows.
    (KnownNat cols, KnownNat rows) =>
    IO (Level cols rows)
generateLevel = do
    let width = fromInteger $ natVal (Proxy @cols) - 1
        height = fromInteger $ natVal (Proxy @rows) - 1
        boundingRectangle = ((0, 0), (height, width))
        initial = Leaf boundingRectangle
        allWalls = listArray boundingRectangle (repeat Wall)
    flip fix initial $ \loop tree -> do
        tree' <- splitTree tree
        if leaves tree' <= 5
            then loop tree'
            else do
                tree'' <- traverse shrinkWalls tree' <$> uniformRIO (0.3, 0.4)
                let rooms = getRooms tree'' --                   Get the rooms of the (shrunken) binary tree
                    centers = map center $ flatten tree'' --     Get the center of each room
                    tunnels = concatMap dig $ mst centers --     Use the rooms' centers to calculate an MST between them
                    (up, down) = generateStairs $ mst centers -- Place the up- and downwards staircases as long away from each other as possible
                    floorCells = concatMap indices rooms --      Positions where items and monsters can be placed

                -- Placing chests and monsters
                monstersAndPositions <- generateByRatioFromPositions @Monster 0.05 floorCells
                chests <- generateByRatioFromPositions @Chest 0.005 floorCells
                let chests' = map (Chest <$>) chests
                    monsters =
                        map (uncurry (position .~)) --                    Assign each monster a position
                            . filter (\(c, _) -> c /= up && c /= down) -- Don't place monsters on the stairs
                            $ monstersAndPositions

                    -- Determine whcih cells to paint over
                    -- (As suggested by the name, order matters here, as we use painter's algorithm)
                    cellsToPaint = concatMap assocs rooms <> chests' <> tunnels <> [(up, Stair Upwards), (down, Stair Downwards)]
                    visibleCells = listArray boundingRectangle (repeat Unseen) // map (,Visible) (surrounding up)

                return $ Level (allWalls // cellsToPaint) visibleCells up down monsters
