{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

module BinaryTree where

import Control.Applicative
import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Function
import World.Tree

-- | Flattening a binary tree is the default 'toList' implementation of 'Foldable'.
prop_flattenIsToList :: BinaryTree Int -> Property
prop_flattenIsToList tree = flatten tree === toList tree

-- Functor laws
-- Gracefully borrowed from: https://austinrochford.com/posts/2014-05-27-quickcheck-laws.html

-- | Functor identity law
prop_functorIdentity :: BinaryTree Int -> Property
prop_functorIdentity tree = fmap id tree === tree

-- | Functor composition law
prop_functorComposition :: BinaryTree Int -> Fun Int Char -> Fun Char String -> Property
prop_functorComposition tree (apply -> f) (apply -> g) =
    fmap (g . f) tree === (fmap g . fmap f) tree

instance Arbitrary a => Arbitrary (BinaryTree a) where
    arbitrary = do
        isBranch <- arbitrary @Bool
        if isBranch
            then liftA2 (:-:) arbitrary arbitrary
            else Leaf <$> arbitrary