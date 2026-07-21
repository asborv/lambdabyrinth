{- |
Module      : Utils.Zipper
Description : A list zipper with basic traversal
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Utils.Zipper
    ( -- * Type
      Zipper(..)
      -- * Traversal
    , goLeft
    , goRight
    , middleIndex
      -- * List conversion
    , fromList
    , toList
    , fromNonEmpty
    , toNonEmpty
    ) where

import Data.List.NonEmpty (NonEmpty((:|)))

data Zipper a = Zipper
    { left   :: [a]
    , middle :: a
    , right  :: [a]
    }
    deriving (Show)


instance Functor Zipper where
    fmap f (Zipper ls m rs) = Zipper (fmap f ls) (f m) (fmap f rs)


-- | Move one step to the right, truncated at the end
goRight :: Zipper a -> Zipper a
goRight z@(Zipper _  _ [])       = z
goRight   (Zipper ls m (r : rs)) = Zipper (m : ls) r rs

-- | Corresponding to @goRight@, but in the opposite direction
goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _ _)        = z
goLeft   (Zipper (l : ls) m rs) = Zipper ls l (m : rs)

-- | Index of the @middle@ element
middleIndex :: Zipper a -> Int
middleIndex   (Zipper []      _ _) = 0
middleIndex z@(Zipper (_ : _) _ _) = 1 + middleIndex (goLeft z)


{- |
Construct a @Zipper@ focused on the first element
of a list, if not empty
-}
fromList :: [a] -> Maybe (Zipper a)
fromList []       = Nothing
fromList (x : xs) = Just $ Zipper [] x xs

-- | Construct a list by concatenating left, middle, and right
toList :: Zipper a -> [a]
toList (Zipper ls m rs) = ls <> [m] <> rs

-- | Similar to @fromList@
fromNonEmpty :: NonEmpty a -> Zipper a
fromNonEmpty (x :| xs) = Zipper [] x xs

-- | Similar to @toList@
toNonEmpty :: Zipper a -> NonEmpty a
toNonEmpty (Zipper []       m rs) = m :| rs
toNonEmpty (Zipper (l : ls) m rs) = l :| ls <> [m] <> rs
