module World.WorldGeneration (create) where

import Control.Monad.Fix (fix)
import GHC.Arr (Array, assocs, bounds, listArray, (//))
import System.Random (Random (random, randomR), randomIO, randomRIO)
import World.World (Cell (..), Coordinate)

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
split (Leaf r@(upperLeft, lowerRight)) = do
    splitRatio <- randomRIO (0.3, 0.7) :: IO Double
    direction <- randomRIO (Vertical, Horizontal)
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

        lowerRight' = case direction of
            Vertical -> (fst lowerRight, splitPoint)
            Horizontal -> (splitPoint, snd lowerRight)
        left = (upperLeft, lowerRight')
        right = (upperLeft', lowerRight)
        upperLeft' = case direction of
            Vertical -> (fst upperLeft, splitPoint)
            Horizontal -> (splitPoint, snd upperLeft)
    print $
        "Leaf "
            <> show r
            <> ". Splitting "
            <> show direction
            <> "ly at "
            <> show splitPoint
            <> "."
    print $ "After split: " <> show ((Leaf left) :-: (Leaf right))
    return $ (Leaf left) :-: (Leaf right)

combine :: BinaryTree Rectangle -> Array Coordinate Cell
combine (Leaf rectangle) = listArray rectangle (repeat Floor)
-- fill bounding rect with walls, and fill in the rooms recursively with rooms (using (//) or something)
combine (left :-: right) =
    listArray ((y0, x0), (y1, x1)) (repeat Wall)
        // assocs leftRoom
        // assocs rightRoom
  where
    leftRoom = combine left
    rightRoom = combine right
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
    return $ Leaf ((y0 + 1, x0 + 1), (y1 - 1, x1 - 1))

-- Leaf
--     (
--         ( round (fromIntegral y0 + ratio * height)
--         , round (fromIntegral x0 + ratio * width)
--         )
--     ,
--         ( round (fromIntegral y1 - ratio * height)
--         , round (fromIntegral x1 - ratio * width)
--         )
--     )

leafs :: BinaryTree a -> Int
leafs (Leaf _) = 1
leafs (left :-: right) = leafs left + leafs right

create :: Int -> Int -> IO (Array Coordinate Cell)
create width height = do
    let boundingRectangle = ((0, 0), (height - 1, width - 1))
    let initial = Leaf boundingRectangle
    flip fix initial $ \loop tree -> do
        tree' <- split tree
        print (leafs tree')
        if leafs tree' <= 8
            then loop tree'
            else do
                tree'' <- shrinkWalls tree'
                print tree''
                return $ listArray boundingRectangle (repeat Wall) // assocs (combine tree'')
