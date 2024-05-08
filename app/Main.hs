module Main where

import Config
import Options.Applicative (execParser)
import Scenes.CreateCharacter (createCharacter)
import Scenes.Game.Scene (playGame)
import Scenes.Result (showResult)

main :: IO ()
main = do
    options <- execParser configOptions
    character <- createCharacter
    playGame character options >>= showResult
    -- If user quits, just return
    -- Otherwise, run the game with the created character
    -- case character of
    --     Nothing -> return ()
    --     Just c -> playGame c options >>= showResult
