module World.WorldGeneration (create) where

import Control.Lens ((.~))
import Control.Monad.Fix (fix)
import Creatures.Monsters (Monster, position, zombie)
import Data.Bifoldable (biList)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (maximumBy, minimumBy)
import GHC.Arr (Array, assocs, indices, listArray, (//))
import System.Random (Random (random, randomR), randomIO, randomRIO)
import World.Cells
import World.Level (Coordinate, Level (..))

-- | Binary tree with data only in its leaves
data BinaryTree a
    = Leaf a
    | (BinaryTree a) :-: (BinaryTree a)
    deriving (Show)

data Direction = Vertical | Horizontal deriving (Show)

instance Random Direction where
    random g = case randomR (True, False) g of
        (False, g') -> (Vertical, g')
        (True, g') -> (Horizontal, g')
    randomR _ = random

-- | A rectangle that is defined by its upper right and lower right corners
type Rectangle = (Coordinate, Coordinate)

type Edge = (Coordinate, Coordinate)

split :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
split (left :-: right) = do
    shouldSplitLeft <- randomIO :: IO Bool
    if shouldSplitLeft
        then split left <&> (:-: right)
        else split right <&> (:-: left)
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

shrinkWalls :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
shrinkWalls (left :-: right) = do
    left' <- shrinkWalls left
    right' <- shrinkWalls right
    return $ left' :-: right'
shrinkWalls (Leaf ((y0, x0), (y1, x1))) = do
    ratio <- randomRIO (0.2, 0.3) :: IO Double
    let width = fromIntegral (x1 - x0) :: Double
        height = fromIntegral (y1 - y0) :: Double
        dx = max 1 (round (ratio * width))
        dy = max 1 (round (ratio * height))
    return $ Leaf ((y0 + dy, x0 + dx), (y1 - dy, x1 - dx))

leaves :: BinaryTree a -> Int
leaves (Leaf _) = 1
leaves (left :-: right) = leaves left + leaves right

flatten :: BinaryTree a -> [a]
flatten (Leaf a) = [a]
flatten (left :-: right) = flatten left <> flatten right

{- | Find the edges that produce the minimum spanning tree for a set of points in a plane.
Candidate edges are all pairs between the nodes that are passed.
Inspired by: https://stackoverflow.com/questions/65546555/implementing-prims-algorithm-in-haskell
-}
mst :: [Coordinate] -> [Edge]
mst [] = []
mst (n : ns) = mst' ns [n] []
  where
    mst' :: [Coordinate] -> [Coordinate] -> [Edge] -> [Edge]
    mst' [] _ edges = edges
    mst' (x : xs) visited edges
        | x `elem` visited = mst' xs visited edges
        | otherwise =
            let shortest = minimumBy (compare `on` weight) ((x,) <$> visited)
             in mst' xs (x : visited) (shortest : edges)

-- The weight of an edge is the distance between its nodes
weight :: Edge -> Double
weight (a, b) = a `distance` b

distance :: Coordinate -> Coordinate -> Double
distance (y0, x0) (y1, x1) = sqrt (fromIntegral (y1 - y0) ^ 2 + fromIntegral (x1 - x0) ^ 2)

center :: Rectangle -> Coordinate
center ((y0, x0), (y1, x1)) = ((y0 + y1) `div` 2, (x0 + x1) `div` 2)

-- | Dig an L-shaped tunnel between two nodes
dig :: Edge -> [(Coordinate, Cell)]
dig ((y0, x0), (y1, x1)) = map (,Tunnel) $ vertical <> horizontal
  where
    vertical = [(y, x0) | y <- [min y0 y1 .. max y0 y1]]
    horizontal = [(y1, x) | x <- [min x0 x1 .. max x0 x1]]

generateMonsters :: [Coordinate] -> IO [Monster]
generateMonsters cells = do
    rs <- mapM (const randomIO) cells :: IO [Double]
    let cellsToPopulate = map fst $ filter ((> 0.95) . snd) (zip cells rs)
    return $ map (\c -> zombie & position .~ c) cellsToPopulate

{- | Count the number of occurrences of a specific element in a list
Courtesy of: https://stackoverflow.com/questions/19554984/haskell-count-occurrences-function
-}
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

{- | Generate staircases, one up, one down.
They are placed at dead ends (leaves) of the provided edges, as distant as possible away from each other.
(Distance is measured in diagonal pixels, not neccesarily the longes path).
-}
generateStairs :: [Edge] -> ((Coordinate, Cell), (Coordinate, Cell))
generateStairs edges = (\(a, b) -> ((a, Stair Upwards), (b, Stair Downwards))) longest
  where
    nodes = concatMap biList edges
    deadEnds = filter (\node -> count node nodes == 1) nodes
    longest =
        maximumBy
            (compare `on` weight)
            [(a, b) | a <- deadEnds, b <- deadEnds]

create :: Int -> Int -> IO Level
create width height = do
    let boundingRectangle = ((0, 0), (height - 1, width - 1))
        initial = Leaf boundingRectangle
        allWalls = listArray boundingRectangle (repeat Wall)
    flip fix initial $ \loop tree -> do
        tree' <- split tree
        if leaves tree' <= 6
            then loop tree'
            else do
                tree'' <- shrinkWalls tree'
                let rooms = getRooms tree'' --                   Get the rooms of the (shrunken) binary tree
                    centers = map center $ flatten tree'' --     Get the center of each room
                    tunnels = concatMap dig $ mst centers --     Use the rooms' centers to calculate an MST between them
                    (up, down) = generateStairs $ mst centers -- Place the up- and downwards staircases as long away from each other as possible
                    levelCells = foldl (//) (allWalls // tunnels) (assocs <$> rooms) // [up] // [down]

                monsters <- generateMonsters (concatMap indices rooms)

                return $ Level levelCells monsters
