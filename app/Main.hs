module Main where

import Scenes.CreateCharacter (createCharacter)
import Scenes.Game (playGame)
import Scenes.Result (showResult)

main :: IO ()
main = do
    character <- createCharacter
    case character of
        Nothing -> return ()
        Just c -> do
            fin <- playGame c
            showResult fin

-- finalState <- playGame character showResult finalState
