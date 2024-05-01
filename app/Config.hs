module Config (Config (..), Difficulty (..), configOptions) where

import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , fullDesc
    , header
    , help
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , switch
    , value
    )

data Difficulty = Easy | Medium | Hard deriving (Read)
data Config = Config {asciiOnly :: Bool, difficulty :: Difficulty}

configOptions :: ParserInfo Config
configOptions = info opts description
  where
    opts = Config <$> asciiFlag <*> difficultyOption
    description =
        fullDesc
            <> progDesc "A Rogue-inspired dungeon crawler"
            <> header "Λbyrinth"

asciiFlag :: Parser Bool
asciiFlag =
    switch
        ( short 'a'
            <> long "ascii-only"
            <> help "Use only ASCII characters for drawing the game"
        )

difficultyOption :: Parser Difficulty
difficultyOption =
    option
        auto
        ( short 'd'
            <> long "difficulty"
            <> help "Set the difficulty of the game (Easy | Medium | Hard)"
            <> metavar "DIFFICULTY"
            <> value Easy
        )
