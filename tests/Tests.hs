module Tests (tests) where

import BinaryTree
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck

tests :: IO [Test]
tests =
    return
        [ getPropertyTest
            PropertyTest
                { name = "Flattening binary tree is the same as toList"
                , tags = []
                , property = prop_flattenIsToList
                }
        , getPropertyTest
            PropertyTest
                { name = "Functor identity law"
                , tags = ["Functor"]
                , property = prop_functorIdentity
                }
        , getPropertyTest
            PropertyTest
                { name = "Functor composition law"
                , tags = ["Functor"]
                , property = prop_functorComposition
                }
        ]
