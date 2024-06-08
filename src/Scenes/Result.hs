{- |
Module      :  Scenes.Result
Description :  Scene for displaying the result of the game
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Result where

import Brick
    ( App (..)
    , BrickEvent (VtyEvent)
    , Widget
    , attrMap
    , defaultMain
    , hLimit
    , halt
    , padAll
    , txt
    , vBox
    , vLimit
    )
import Brick.Main (neverShowCursor)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Control.Lens ((^.))
import Control.Monad (void)
import Creatures.Player
import qualified Data.Text as T
import Graphics.Vty (Event (..), defAttr)
import Types
import Utils

type Name = ()

app :: Scene GameState Name
app =
    App
        { appDraw = drawScene
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey _ [] -> halt
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

drawScene :: GameState -> [Widget Name]
drawScene game =
    [ center
        . vLimit 10
        . hLimit 30
        . borderWithLabel (txt "Results")
        . padAll 1
        $ centerLines
            [ "YOU DIED!"
            , game ^. player . name <> " got to level " <> tshow (game ^. currentLevel)
            , "Press any key to quit"
            ]
    ]

-- | Take a list of 'Text's, and center each in a vertical list
centerLines :: [T.Text] -> Widget Name
centerLines = vBox . fmap (hCenter . txt)

showResult :: GameState -> IO ()
showResult = void . defaultMain app
