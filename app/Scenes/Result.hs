module Scenes.Result where

import Scenes.Game (GameState)

showResult :: GameState -> IO ()
showResult = print