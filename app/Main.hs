module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import Creatures.Player
import Data.List.Split
import qualified Data.Map as Map
import Draw
import GHC.Arr
import Graphics.Vty
import World

type Name = ()

data GameState = GameState
    { _player :: Player
    , _currentLevel :: Int
    , _world :: World
    }
    deriving (Show)

makeLenses ''GameState

app :: App GameState () Name
app =
    App
        { appDraw = drawGame
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey (KChar 'q') [] -> halt
                -- Movement
                EvKey (KChar 'w') [] -> modify (player . pos %~ \(y, x) -> (y - 1, x))
                EvKey (KChar 'a') [] -> modify (player . pos %~ \(y, x) -> (y, x - 1))
                EvKey (KChar 's') [] -> modify (player . pos %~ \(y, x) -> (y + 1, x))
                EvKey (KChar 'd') [] -> modify (player . pos %~ \(y, x) -> (y, x + 1))
                -- Manual level select (DEBUGGING)
                EvKey (KChar 'b') [] -> modify (currentLevel %~ (+ 1))
                EvKey (KChar 'B') [] -> modify (currentLevel %~ subtract 1)
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

drawGame :: GameState -> [Widget Name]
drawGame game =
    let ui = drawLog game <+> (drawLevel game <=> drawStats game) <+> drawEquipment game
     in [ui]

drawLog :: GameState -> Widget Name
drawLog _ = border $ hLimit 10 $ center $ txt "Log"

drawStats :: GameState -> Widget Name
drawStats _ = border $ vLimit 3 $ center $ txt "Stats"

drawEquipment :: GameState -> Widget Name
drawEquipment _ = border $ hLimit 20 $ center $ txt "Equipment"

drawLevel :: GameState -> Widget Name
drawLevel game = borderWithLabel (txt "Lambdabyrinth") $ center $ vBox (hBox <$> rows)
  where
    level = (game ^. world) !! (game ^. currentLevel)
    rows = chunksOf (width level) $ do
        (coord, cell) <- level ^. cells & assocs
        let monster = Map.lookup coord (level ^. monsters)

        return $
            if game ^. player . pos == coord
                then draw $ game ^. player
                else maybe (draw cell) draw monster

main :: IO ()
main = do
    let initialState = GameState mrBean 0 [emptyLevel, firstLevel]
    finalState <- defaultMain app initialState
    print finalState

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
