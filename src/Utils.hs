module Utils where

import Control.Monad (when)
import Data.Text (Text, pack)

tshow :: Show a => a -> Text
tshow = pack . show

guarded :: Monad m => m Bool -> m () -> m ()
guarded condition action = condition >>= flip when action
