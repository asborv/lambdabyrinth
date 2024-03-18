module Main where

import Brick
import Brick.Widgets.Center
import Control.Lens (to, (%~), (^.))
import Control.Lens.Lens ((&))
import Control.Lens.TH
import Creatures.Player
import Data.List.Split
import qualified Data.Map as Map
import GHC.Arr
import Graphics.Vty
import Items
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
                EvKey (KChar 'b') [] -> modify (currentLevel %~ (+ 1))
                EvKey (KChar 'B') [] -> modify (currentLevel %~ subtract 1)
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

drawGame :: GameState -> [Widget Name]
drawGame game = [center $ drawLevel game]

drawLevel :: GameState -> Widget Name
drawLevel game = vBox (hBox <$> rows)
  where
    level = (game ^. world) !! (game ^. currentLevel)
    rows = chunksOf (width level) $ do
        (coord, cell) <- level ^. cells & assocs
        let monster = level ^. monsters . to (Map.lookup coord)
        return $
            if (game ^. player . pos) == coord
                then str $ show (game ^. player)
                else str $ maybe (show cell) show monster

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
        , _inventory = [Armour Iron Helmet, Weapon Wood Sword]
        , _health = 10
        , _characterClass = Wizard
        }
