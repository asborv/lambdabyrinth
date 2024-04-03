module Main where

import Scenes.CreateCharacter (createCharacter)
import Scenes.Game (playGame)
import Scenes.Result (showResult)

main :: IO ()
main = do
    character <- createCharacter

    -- If user quits, just return
    -- Otherwise, run the game with the created character
    case character of
        Nothing -> return ()
        Just c -> playGame c >>= showResult
