module Scenes.CreateCharacter where

import Creatures.Player (Class (..), Player (..))

mrBean :: Player
mrBean =
    Player
        { _name = "Mr. Bean"
        , _pos = (0, 0)
        , _hand = Nothing
        , _helmet = Nothing
        , _cuirass = Nothing
        , _gloves = Nothing
        , _boots = Nothing
        , _health = 10
        , _characterClass = Wizard
        }

createCharacter :: IO Player
createCharacter = return mrBean
