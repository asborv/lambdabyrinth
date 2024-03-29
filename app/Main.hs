module Main where

import Scenes.CreateCharacter (createCharacter)
import Scenes.Game (playGame)
import Scenes.Result (showResult)

main :: IO ()
main = do
    character <- createCharacter
    finalState <- playGame character
    showResult finalState
