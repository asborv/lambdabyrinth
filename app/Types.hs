{- |
Module     : Types
Description: Contains the data types used in the game.
maintainer : asbjorn.orvedal@gmail.com
-}

module Types where
import qualified Creatures.Player as P
import World.Level
import Control.Lens.TH (makeLenses)
import Data.Text (Text)

data GameState = GameState
    { _player :: P.Player
    , _currentLevel :: Int
    , _world :: World
    , _history :: [Text]
    }

makeLenses ''GameState
