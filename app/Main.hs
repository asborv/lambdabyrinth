module Main where

import Brick (
    App (..),
    BrickEvent (VtyEvent),
    Widget,
    attrMap,
    defaultMain,
    hBox,
    hLimit,
    txt,
    vBox,
    vLimit,
    (<+>),
    (<=>),
 )
import Brick.Main (halt, neverShowCursor)
import Brick.Types (modify)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Creatures.Player
import Data.List.Split
import qualified Data.Map as Map
import Draw
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
                EvKey (KChar 'w') [] -> modify (move North)
                EvKey (KChar 'a') [] -> modify (move West)
                EvKey (KChar 's') [] -> modify (move South)
                EvKey (KChar 'd') [] -> modify (move East)
                -- Manual level select (DEBUGGING)
                EvKey (KChar 'b') [] -> modify (currentLevel %~ (+ 1))
                EvKey (KChar 'B') [] -> modify (currentLevel %~ subtract 1)
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

move :: Direction -> GameState -> GameState
move direction game
    | target `elem` indices (level ^. cells)
    , isTraversible cell =
        game & player . pos .~ target
    | otherwise = game
  where
    (y, x) = game ^. player . pos
    target = case direction of
        North -> (y - 1, x)
        East -> (y, x + 1)
        South -> (y + 1, x)
        West -> (y, x - 1)
    level = (game ^. world) !! (game ^. currentLevel)
    cell = (level ^. cells) ! target

drawGame :: GameState -> [Widget Name]
drawGame game =
    let ui = drawLog game <+> (drawLevel game <=> drawStats game) <+> drawEquipment game
     in [ui]

drawLog :: GameState -> Widget Name
drawLog _ = border $ hLimit 10 $ center $ txt "Log"

drawStats :: GameState -> Widget Name
drawStats _ = border $ vLimit 3 $ center $ txt "Stats"

drawEquipment :: GameState -> Widget Name
drawEquipment game = border $ hLimit 20 $ center $ vBox slots
  where
    slots       = [handSlot, helmetSlot, cuirassSlot, glovesSlot, bootsSlot]
    handSlot    = itemSlot (game ^. player . hand)
    helmetSlot  = itemSlot (game ^. player . helmet)
    cuirassSlot = itemSlot (game ^. player . cuirass)
    glovesSlot  = itemSlot (game ^. player . gloves)
    bootsSlot   = itemSlot (game ^. player . boots)

    itemSlot :: Drawable a => Maybe a -> Widget Name
    itemSlot Nothing = border $ txt "    "
    itemSlot (Just item) = border (draw item)

drawLevel :: GameState -> Widget Name
drawLevel game = borderWithLabel (txt "Lambdabyrinth") $ center $ vBox (hBox <$> rows)
  where
    level = (game ^. world) !! (game ^. currentLevel)
    rows  = chunksOf (width level) $ do
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
        , _hand = Just (Dagger Diamond)
        , _helmet = Nothing
        , _cuirass = Nothing
        , _gloves = Nothing
        , _boots = Nothing
        , _health = 10
        , _characterClass = Wizard
        }
