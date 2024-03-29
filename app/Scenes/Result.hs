module Scenes.Result where

import Scenes.Game (GameState)

import Brick
    ( App (..)
    , BrickEvent (VtyEvent)
    , Widget
    , attrMap
    , defaultMain
    , halt
    , txt
    , vBox
    )
import Brick.Main (neverShowCursor)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Data.Text (Text)
import Graphics.Vty (Event (..), defAttr)
import Scenes.Scene (Name, Scene)

app :: Scene ()
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

drawScene :: () -> [Widget Name]
drawScene _ =
    [ borderWithLabel (txt "Results") $
        vCenter $
            centerLines ["💀 YOU DIED!", "Press any key to quit"]
    ]

showResult :: GameState -> IO ()
showResult = return $ defaultMain app ()

-- | Take a list of 'Text's, and center each in a vertical list
centerLines :: [Text] -> Widget Name
centerLines = vBox . fmap (hCenter . txt)
