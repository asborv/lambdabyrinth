{- |
Module     : Types
Description: Contains the data types used in the game.
maintainer : asbjorn.orvedal@gmail.com
-}
module Types where

import Brick (App)
import Control.Lens.TH (makeLenses)
import qualified Creatures.Player as P
import Data.Text (Text)
import World.Level

data GameState = GameState
    { _player :: P.Player
    , _currentLevel :: Int
    , _world :: World 40 40
    , _history :: [Text]
    }

makeLenses ''GameState

type Name = ()
type Scene a = App a () Name