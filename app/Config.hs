module Config (Config (..), Difficulty (..)) where

data Difficulty = Easy | Medium | Hard
data Config = Config {asciiOnly :: Bool, difficulty :: Difficulty}
