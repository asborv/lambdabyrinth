module Main where

import Config
import Options.Applicative (execParser)
import Scenes.CreateCharacter (createCharacter)
import Scenes.Game.Scene (playGame)
import Scenes.Result (showResult)

main :: IO ()
main = do
    options <- execParser configOptions
    createCharacter >>= playGame options >>= showResult
