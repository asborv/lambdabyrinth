module Scenes.CreateCharacter (createCharacter) where

import Brick
    ( App (..)
    , BrickEvent (VtyEvent)
    , Widget
    , attrMap
    , defaultMain
    , halt
    , txt
    )
import Brick.Main (neverShowCursor)
import Brick.Types (modify)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Creatures.Player (Class (..), Player (..))
import Graphics.Vty (Event (..), Key (KChar, KEnter), defAttr)
import Scenes.Scene (Name, Scene)

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

app :: Scene (Maybe Player)
app =
    App
        { appDraw = drawScene
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                -- Quit
                EvKey (KChar 'q') [] -> modify (const Nothing) >> halt
                -- Continue with Mr. Bean
                EvKey KEnter [] -> modify (const $ Just mrBean) >> halt
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

drawScene :: Maybe Player -> [Widget Name]
drawScene _ = [borderWithLabel (txt "Create character") $ center $ txt "hello"]

createCharacter :: IO (Maybe Player)
createCharacter = do
    defaultMain app Nothing
