module World.WorldGeneration (create) where

import Control.Monad.Fix (fix)
import GHC.Arr (Array, assocs, bounds, listArray, (//))
import System.Random (Random (random, randomR), randomIO, randomRIO)
import World.World (Cell (..), Coordinate, Level (..), World)

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
    randomR _ g = random g

-- | A rectangle that is defined by its upper right and lower right corners
type Rectangle = (Coordinate, Coordinate)

split :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
split (left :-: right) = do
    shouldSplitLeft <- randomIO :: IO Bool
    if shouldSplitLeft
        then split left >>= return . (:-: right)
        else split right >>= return . (left :-:)
split (Leaf (upperLeft, lowerRight)) = do
    splitRatio <- randomRIO (0.4, 0.6) :: IO Double
    direction <- randomIO :: IO Direction

    -- The available room for splitting
    let splitBasis = fromIntegral $ case direction of
            Vertical -> (snd lowerRight - snd upperLeft)
            Horizontal -> (fst lowerRight - fst upperLeft)

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

    return $ (Leaf left) :-: (Leaf right)

toCells :: BinaryTree Rectangle -> Array Coordinate Cell
toCells (Leaf rectangle) = listArray rectangle (repeat Floor)
toCells (left :-: right) =
    listArray ((y0, x0), (y1, x1)) (repeat Wall)
        // assocs leftRoom
        // assocs rightRoom
  where
    leftRoom = toCells left
    rightRoom = toCells right
    x0 = min (snd $ fst $ bounds leftRoom) (snd $ fst $ bounds rightRoom)
    y0 = min (fst $ fst $ bounds leftRoom) (fst $ fst $ bounds rightRoom)
    x1 = max (snd $ snd $ bounds leftRoom) (snd $ snd $ bounds rightRoom)
    y1 = max (fst $ snd $ bounds leftRoom) (fst $ snd $ bounds rightRoom)

shrinkWalls :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
shrinkWalls (left :-: right) = do
    left' <- shrinkWalls left
    right' <- shrinkWalls right
    return $ left' :-: right'
shrinkWalls (Leaf ((y0, x0), (y1, x1))) = do
    ratio <- randomRIO (0.1, 0.15) :: IO Double
    let width = fromIntegral (x1 - x0) :: Double
        height = fromIntegral (y1 - y0) :: Double
        dx = max 1 (round (ratio * width))
        dy = max 1 (round (ratio * height))
    return $ Leaf (((y0 + dy), (x0 + dx)), ((y1 - dy), (x1 - dx)))

leaves :: BinaryTree a -> Int
leaves (Leaf _) = 1
leaves (left :-: right) = leaves left + leaves right

create :: Int -> Int -> IO Level
create width height = do
    let boundingRectangle = ((0, 0), (height - 1, width - 1))
        initial = Leaf boundingRectangle
        allWalls = listArray boundingRectangle (repeat Wall)
    flip fix initial $ \loop tree -> do
        tree' <- split tree
        if leaves tree' <= 10
            then loop tree'
            else do
                tree'' <- shrinkWalls tree'
                let cells = toCells tree''
                return $ Level (allWalls // assocs cells) []