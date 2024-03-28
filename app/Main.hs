module Main where

import Brick
    ( App (..)
    , BrickEvent (VtyEvent)
    , EventM
    , Widget
    , attrMap
    , defaultMain
    , hBox
    , hLimit
    , txt
    , vBox
    , vLimit
    , (<+>)
    , (<=>)
    )
import Brick.Main (halt, neverShowCursor)
import Brick.Types (modify)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Arrow ((>>>))
import Control.Lens (element, makeLenses, use, (%~), (&), (.~), (^.))
import Control.Lens.Combinators (to)
import Control.Monad (when)
import Creatures.Combatant
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.List (find, intercalate)
import Data.List.Split
import qualified Data.Text as T
import Draw
import GHC.Arr
import Graphics.Vty
import World

type Name = ()
type GameEvent = EventM Name GameState ()

data GameState = GameState
    { _player :: P.Player
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
                EvKey (KChar 'w') [] -> playerMove North
                EvKey (KChar 'a') [] -> playerMove West
                EvKey (KChar 's') [] -> playerMove South
                EvKey (KChar 'd') [] -> playerMove East
                -- Manual level select (DEBUGGING)
                EvKey (KChar 'b') [] -> modify (currentLevel %~ (+ 1))
                EvKey (KChar 'B') [] -> modify (currentLevel %~ subtract 1)
                EvKey (KChar 'R') [] -> modify (world . element 0 . monsters %~ (M.zombie :))
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

{- | Move the player in the specified direction.
Accept a `Direction` and perform the necessary
checks to determine if the player can move in that direction.
If the target cell is a valid cell and is traversable,
the player's position is updated accordingly. Otherwise, nothing happens.
-}
playerMove :: Direction -> GameEvent
playerMove direction = do
    -- Get the player's position and the current level
    (y, x) <- use (player . P.pos)
    curr <- use currentLevel
    level <- use (world . to (!! curr))
    let levelCells = level ^. cells
        cell = levelCells ! target
        isLegalMove = target `elem` indices levelCells && isTraversible cell
        target = case direction of
            North -> (y - 1, x)
            East -> (y, x + 1)
            South -> (y + 1, x)
            West -> (y, x - 1)

    -- Update the player's position only when the movement is legal
    when isLegalMove $ do
        let monster = find (\m -> m ^. M.position == target) (level ^. monsters)
        case monster of
            (Just m) -> playerAttackEvent m
            Nothing -> modify (player . P.pos .~ target)
        reactToPlayerMove cell

playerAttackEvent :: M.Monster -> GameEvent
playerAttackEvent monster = do
    me <- use player
    curr <- use currentLevel
    everyone <- use (world . element curr . monsters)

    -- All other monsters than the target
    let others = filter (/= monster) everyone

    -- The monster after being attacked
    let monster' = me `attack` monster

    -- The remaining monsters after attacking the target
    let remaining =
            if monster' ^. M.health <= 0
                then others
                else monster' : others

    modify $ world . element curr . monsters .~ remaining

{- | Modify the game state as a reaction to a player entering a cell
(1) Increment/decrement level for staircases
-}
reactToPlayerMove :: Cell -> GameEvent
reactToPlayerMove (Stair Downwards) = modify (currentLevel %~ (+ 1))
reactToPlayerMove (Stair Upwards) = modify (currentLevel %~ subtract 1)
reactToPlayerMove _ = return ()

drawGame :: GameState -> [Widget Name]
drawGame game =
    let ui = drawLog game <+> (drawLevel game <=> drawStats game) <+> drawEquipment game
     in [ui]

drawLog :: GameState -> Widget Name
drawLog _ = border $ hLimit 10 $ center $ txt "Log"

drawStats :: GameState -> Widget Name
drawStats game =
    border
        $ vLimit 3
        $ center
        $ txt
        $ T.pack
        $ intercalate
            ", "
        $ game
            ^. world
                . element (game ^. currentLevel)
                . monsters
                . to (map ((^. M.health) >>> show))

drawEquipment :: GameState -> Widget Name
drawEquipment game = border $ hLimit 20 $ center $ vBox slots
  where
    slots = [handSlot, helmetSlot, cuirassSlot, glovesSlot, bootsSlot]
    handSlot = itemSlot (game ^. player . P.hand)
    helmetSlot = itemSlot (game ^. player . P.helmet)
    cuirassSlot = itemSlot (game ^. player . P.cuirass)
    glovesSlot = itemSlot (game ^. player . P.gloves)
    bootsSlot = itemSlot (game ^. player . P.boots)

    itemSlot :: Drawable a => Maybe a -> Widget Name
    itemSlot Nothing = border $ txt "    "
    itemSlot (Just item) = border (draw item)

drawLevel :: GameState -> Widget Name
drawLevel game = borderWithLabel (txt "Lambdabyrinth") $ center $ vBox (hBox <$> rows)
  where
    level = (game ^. world) !! (game ^. currentLevel)
    rows = chunksOf (width level) $ do
        (coord, cell) <- level ^. cells & assocs
        let monster = find (\m -> m ^. M.position == coord) (level ^. monsters)

        return $
            if game ^. player . P.pos == coord
                then draw $ game ^. player
                else maybe (draw cell) draw monster

main :: IO ()
main = do
    let initialState = GameState mrBean 0 [emptyLevel, firstLevel]
    finalState <- defaultMain app initialState
    print finalState

mrBean :: P.Player
mrBean =
    P.Player
        { P._name = "Mr. Bean"
        , P._pos = (0, 0)
        , P._hand = Nothing
        , P._helmet = Nothing
        , P._cuirass = Nothing
        , P._gloves = Nothing
        , P._boots = Nothing
        , P._health = 10
        , P._characterClass = P.Wizard
        }
