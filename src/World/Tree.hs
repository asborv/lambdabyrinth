module World.Tree where

import Data.Foldable (minimumBy)
import Data.Function (on)
import World.Level (Coordinate)
import Control.Applicative (liftA2)

type Edge = (Coordinate, Coordinate)

-- | Binary tree with data only in its leaves
data BinaryTree a
    = Leaf a
    | (BinaryTree a) :-: (BinaryTree a)
    deriving (Eq, Show)

instance Functor BinaryTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (left :-: right) = fmap f left :-: fmap f right

instance Foldable BinaryTree where
    foldMap f (Leaf a) = f a
    foldMap f (left :-: right) = foldMap f left <> foldMap f right

instance Traversable BinaryTree where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (left :-: right) = liftA2 (:-:) (traverse f left) (traverse f right)

-- | Cound the number of leaves in a binary tree
leaves :: BinaryTree a -> Int
leaves = foldr (const (+ 1)) 0

-- | Flatten a binary tree into a list, left-first DFS order
flatten :: BinaryTree a -> [a]
flatten = foldr (:) []

-- | Euclidean distance between two points
distance :: Coordinate -> Coordinate -> Double
distance (y0, x0) (y1, x1) = sqrt (dx + dy)
  where
    dx = fromIntegral (y1 - y0) ^ 2
    dy = fromIntegral (x1 - x0) ^ 2

-- The weight of an edge is the distance between its nodes
weight :: Edge -> Double
weight (a, b) = a `distance` b

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
            let shortest = minimumBy (compare `on` weight) candidates
                candidates = (x,) <$> visited
             in mst' xs (x : visited) (shortest : edges)
