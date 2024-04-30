{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Scenes.Game where

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
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
    ( element
    , makeLenses
    , use
    , (%=)
    , (&)
    , (+=)
    , (-=)
    , (.=)
    , (^.)
    )
import Control.Lens.Combinators (to)
import Control.Lens.Operators ((.~))
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Creatures.Combatant
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.List (find)
import Data.List.Split
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Draw
import GHC.Arr
import Graphics.Vty
import HaskellWorks.Control.Monad.Lazy (interleaveSequenceIO)
import Scenes.Scene
import World.World
import World.WorldGeneration (create)

type GameEvent a = ReaderT Config (EventM Name GameState) a

data GameState = GameState
    { _player :: P.Player
    , _currentLevel :: Int
    , _world :: World
    }

makeLenses ''GameState

app :: Config -> Scene GameState
app config@(Config asciiOnly) =
    App
        { appDraw = drawGame asciiOnly
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey (KChar 'q') [] -> halt
                -- Movement
                EvKey (KChar 'w') [] -> flip runReaderT config $ playerMove North
                EvKey (KChar 'a') [] -> flip runReaderT config $ playerMove West
                EvKey (KChar 's') [] -> flip runReaderT config $ playerMove South
                EvKey (KChar 'd') [] -> flip runReaderT config $ playerMove East
                -- Manual level select (DEBUGGING)
                EvKey (KChar 'b') [] -> currentLevel += 1
                EvKey (KChar 'B') [] -> currentLevel -= 1
                EvKey (KChar 'R') [] -> world . element 0 . monsters %= (M.zombie :)
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
playerMove :: Direction -> GameEvent ()
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
            Nothing -> player . P.pos .= target
        environmentReactEvent cell

    me <- use player
    when (me ^. P.health <= 0) (lift halt)

{- | Modify the game state as a reaction to a player entering a cell
(1) Increment/decrement level for staircases
-}
environmentReactEvent :: Cell -> GameEvent ()
environmentReactEvent (Stair Downwards) = do
    -- Go to next level
    currentLevel += 1

    -- Find where the upwards stairs are on the next level, place player there
    stairsUp <- getCellPositionM (Stair Upwards)
    maybe (return ()) (\coord -> player . P.pos .= coord) stairsUp
environmentReactEvent (Stair Upwards) = do
    curr <- use currentLevel
    -- Move to previous level only if the player is not on the starting level
    unless (curr <= 0) $ do
        currentLevel -= 1
        stairsDown <- getCellPositionM (Stair Downwards)
        maybe (return ()) (\coord -> player . P.pos .= coord) stairsDown
environmentReactEvent _ = return ()

playerAttackEvent :: M.Monster -> GameEvent ()
playerAttackEvent monster = do
    me <- use player
    curr <- use currentLevel
    everyone <- use (world . to (!! curr) . monsters)

    -- Get target monster, attack it, and grab the remaining monsters
    let others = filter (/= monster) everyone
        monster' = me `attack` monster
        monsterIsAlive = monster' ^. M.health > 0
        remaining =
            if not monsterIsAlive
                then others
                else monster' : others

    -- Update the list of all the monsters
    world . element curr . monsters .= remaining

    -- Have the monster attack the player
    player %= (monster' `attack`)

    -- If the monster is dead, move the player to the position of the deceased monster
    unless monsterIsAlive (player . P.pos .= monster' ^. M.position)

-- | Given the current level, get the first position (if any) of the specified cell type
getCellPositionM :: Cell -> GameEvent (Maybe Coordinate)
getCellPositionM cell = do
    curr <- use currentLevel
    level <- use (world . to (!! curr))
    return $ getCellPosition cell level

getCellPosition :: Cell -> Level -> Maybe Coordinate
getCellPosition cell level = fst <$> find ((== cell) . snd) (assocs $ level ^. cells)

drawGame :: Bool -> GameState -> [Widget Name]
drawGame asciiOnly game =
    let ui =
            drawLog game
                <+> (drawLevel asciiOnly game <=> drawStats game)
                <+> drawEquipment asciiOnly game
     in [ui]

drawLog :: GameState -> Widget Name
drawLog _ = border . hLimit 10 . center $ txt "Log"

drawStats :: GameState -> Widget Name
drawStats game =
    border
        . vLimit 3
        . center
        . txt
        . T.pack
        . show
        $ game ^. player . P.health

drawEquipment :: Bool -> GameState -> Widget Name
drawEquipment asciiOnly game = border . hLimit 20 . center $ vBox slots
  where
    slots = [handSlot, helmetSlot, cuirassSlot, glovesSlot, bootsSlot]
    handSlot = itemSlot (game ^. player . P.hand)
    helmetSlot = itemSlot (game ^. player . P.helmet)
    cuirassSlot = itemSlot (game ^. player . P.cuirass)
    glovesSlot = itemSlot (game ^. player . P.gloves)
    bootsSlot = itemSlot (game ^. player . P.boots)

    itemSlot :: Drawable a => Maybe a -> Widget Name
    itemSlot Nothing = border $ txt "    "
    itemSlot (Just item) = border (draw asciiOnly item)

drawLevel :: Bool -> GameState -> Widget Name
drawLevel asciiOnly game = borderWithLabel (txt "Lambdabyrinth") . center $ vBox (hBox <$> rows)
  where
    level = (game ^. world) !! (game ^. currentLevel)
    rows = chunksOf (width level) $ do
        (coord, cell) <- level ^. cells & assocs
        let monster = find (\m -> m ^. M.position == coord) (level ^. monsters)

        return $
            if game ^. player . P.pos == coord
                then draw asciiOnly $ game ^. player
                else maybe (draw asciiOnly cell) (draw asciiOnly) monster

playGame :: P.Player -> IO GameState
playGame character = do
    (level : ls) <- interleaveSequenceIO $ repeat (create 40 40)
    -- The up- and downwards stairs are guaranteed to exist on each level
    let startingPosition =
            fromMaybe
                (error $ "Did not find " <> show (Stair Upwards) <> " on the first level.")
                (getCellPosition (Stair Upwards) level)
        initialState = GameState (character & P.pos .~ startingPosition) 0 (level : ls)
        config = Config True

    defaultMain (app config) initialState
