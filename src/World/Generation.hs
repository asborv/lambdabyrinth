{- |
Module      : World.Generation
Description : All about generating levels in the game world
Maintainer  : asbjorn.orvedal@gmail.com
-}
module World.Generation (generateLevel) where

import Control.Lens ((.~))
import Control.Monad.Fix (fix)
import Creatures.Monsters (Monster, position)
import Data.Bifoldable (biList)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Data (Proxy (..))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (maximumBy)
import GHC.Arr (Array, assocs, indices, listArray, (//))
import GHC.TypeLits (KnownNat, natVal)
import Items.Chest
import System.Random (Random (random, randomR), randomIO, randomRIO)
import Utils (count)
import World.Cells
import World.Level (Coordinate, Level (..))
import World.Tree

data Direction = Vertical | Horizontal deriving (Show, Bounded, Enum)

instance Random Direction where
    random = randomR (minBound, maxBound)
    randomR (lower, upper) = first toEnum . randomR (fromEnum lower, fromEnum upper)

-- | A rectangle that is defined by its upper right and lower right corners
type Rectangle = (Coordinate, Coordinate)

{- | Split two random leaves in a binary tree into two new leaves
The bisection is done by a random direction and a random point (within a ratio range) along that direction
-}
split :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
split (left :-: right) = randomIO >>= bool (split left <&> (:-: right)) (split right <&> (:-: left))
split (Leaf (upperLeft, lowerRight)) = do
    splitRatio <- randomRIO (0.4, 0.6) :: IO Double
    direction <- randomIO :: IO Direction

    -- The available room for splitting
    let splitBasis = fromIntegral $ case direction of
            Vertical -> snd lowerRight - snd upperLeft
            Horizontal -> fst lowerRight - fst upperLeft

        -- Offset for which to compute the split
        offset = case direction of
            Vertical -> snd upperLeft
            Horizontal -> fst upperLeft

        -- Split at some point in range of the split basis
        splitPoint = round (splitRatio * splitBasis) + offset

        -- The new bounding rectangle corners
        lowerRight' = case direction of
            Vertical -> (fst lowerRight, splitPoint)
            Horizontal -> (splitPoint, snd lowerRight)
        upperLeft' = case direction of
            Vertical -> (fst upperLeft, splitPoint)
            Horizontal -> (splitPoint, snd upperLeft)

        -- The branches to split this leaf into
        left = (upperLeft, lowerRight')
        right = (upperLeft', lowerRight)

    return $ Leaf left :-: Leaf right

-- | Create an array representation of each room in the binarytree's leaves
getRooms :: BinaryTree Rectangle -> [Array Coordinate Cell]
getRooms = map (\rectangle -> listArray rectangle (repeat Floor)) . flatten

{- |  Take a room, and push its walls inwards by a random ratio.
 The shrinking is symmetric on the horizontal and vertical axis, respecitvely.
 (The walls will be pushed in by _at least_ 1 cell)
-}
shrinkWalls :: Rectangle -> IO Rectangle
shrinkWalls ((y0, x0), (y1, x1)) = do
    ratio <- randomRIO (0.2, 0.3) :: IO Double
    let width = fromIntegral (x1 - x0) :: Double
        height = fromIntegral (y1 - y0) :: Double
        dx = max 1 (round (ratio * width))
        dy = max 1 (round (ratio * height))
    return ((y0 + dy, x0 + dx), (y1 - dy, x1 - dx))

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
generateByRatioFromPositions :: Random a => Double -> [Coordinate] -> IO [(Coordinate, a)]
generateByRatioFromPositions ratio cells = do
    rs <- mapM (const randomIO) cells :: IO [Double]
    let cellsToPopulate = map fst $ filter ((< ratio) . snd) (zip cells rs)
    items <- mapM (const randomIO) cellsToPopulate
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
        tree' <- split tree
        if leaves tree' <= 9
            then loop tree'
            else do
                tree'' <- traverse shrinkWalls tree'
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

                return $ Level (allWalls // cellsToPaint) up down monsters
