module Main where

import Config
import Scenes.CreateCharacter (createCharacter)
import Scenes.Game.Scene (playGame)
import Scenes.Result (showResult)
import Options.Applicative (execParser)

main :: IO ()
main = do
    options <- execParser configOptions
    character <- createCharacter

    -- If user quits, just return
    -- Otherwise, run the game with the created character
    case character of
        Nothing -> return ()
        Just c -> playGame c options >>= showResult
