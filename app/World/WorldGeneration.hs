module World.WorldGeneration (create) where

import Control.Monad.Fix (fix)
import GHC.Arr (Array, listArray)
import System.Random (Random (random, randomR), randomRIO)
import World.World (Cell (Wall), Coordinate)

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) (BinaryTree a)
    deriving (Show)

data Direction = Vertical | Horizontal

instance Random Direction where
    random g = case randomR (True, False) g of
        (False, g') -> (Vertical, g')
        (True, g') -> (Horizontal, g')
    randomR _ g = random g

-- | A rectangle that is defined by its upper right and lower right corners
type Rectangle = (Coordinate, Coordinate)

split :: Direction -> Double -> BinaryTree Rectangle -> BinaryTree Rectangle
split direction at (Leaf (upperLeft, lowerRight)) = Branch (Leaf left) (Leaf right)
  where
    splitPoint = case direction of
        Vertical -> round (at * (fromIntegral . fst) lowerRight)
        Horizontal -> round (at * (fromIntegral . snd) lowerRight)
    left = case direction of
        Vertical -> (upperLeft, (fst lowerRight, splitPoint))
        Horizontal -> (upperLeft, (splitPoint, snd lowerRight))
    right =
        ( (fst lowerRight - (fst . snd) left, snd lowerRight - (snd . snd) left)
        , lowerRight
        )
split direction at (Branch _ right) = split direction at right

combine :: BinaryTree Rectangle -> Array Coordinate Cell
combine _ = listArray ((0, 0), (9, 9)) (repeat Wall)

shrinkWalls :: BinaryTree Rectangle -> IO (BinaryTree Rectangle)
shrinkWalls (Branch left right) = do
    left' <- shrinkWalls left
    right' <- shrinkWalls right
    return $ Branch left' right'
shrinkWalls (Leaf ((y0, x0), (y1, x1))) = do
    ratio <- randomRIO (0, 0.3) :: IO Double
    let width = fromIntegral (x1 - x0) :: Double
        height = fromIntegral (y1 - y0) :: Double
    return $
        Leaf
            (
                ( round (fromIntegral y0 + ratio * height)
                , round (fromIntegral x0 + ratio * width)
                )
            ,
                ( round (fromIntegral y1 - ratio * height)
                , round (fromIntegral x1 - ratio * width)
                )
            )

size :: BinaryTree a -> Int
size (Leaf _) = 1
size (Branch left right) = size left + size right

create :: Int -> Int -> IO (Array Coordinate Cell)
create width height = do
    let initial = Leaf ((0, 0), (height - 1, width - 1))
    flip fix initial $ \loop tree -> do
        direction <- randomRIO (Vertical, Horizontal)
        at <- randomRIO (0 :: Double, 1 :: Double)
        tree' <- shrinkWalls $ split direction at tree

        if size tree' < 10
            then loop tree'
            else return $ combine tree'
