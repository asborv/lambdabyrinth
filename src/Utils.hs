module Utils where

import Control.Monad (when)
import Data.Text (Text, pack)

tshow :: Show a => a -> Text
tshow = pack . show

guarded :: Monad m => m Bool -> m () -> m ()
guarded condition action = condition >>= flip when action

{- | Count the number of occurrences of a specific element in a list
Courtesy of: https://stackoverflow.com/questions/19554984/haskell-count-occurrences-function
-}
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- |  Replaces all occurrences of a with b in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)
