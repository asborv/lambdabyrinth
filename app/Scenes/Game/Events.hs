{- |
Module      : Scenes.Game.Events
Description : Player- or environment-triggered events in the game.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Game.Events where

import Brick (EventM, halt)
import Config
import Control.Lens (element, to, use, (+=), (-=), (.=), (^.))
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Creatures.Combatant
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.Foldable (find)
import GHC.Arr (assocs, indices, (!))
import Scenes.Scene
import Types
import World.Cells
import World.Level

type GameEvent a = ReaderT Config (EventM Name GameState) a

{- | Move the player in the specified direction.
Accept a `Direction` and perform the necessary
checks to determine if the player can move in that direction.
If the target cell is a valid cell and is traversable,
the player's position is updated accordingly. Otherwise, nothing happens.
-}
moveEvent :: Direction -> GameEvent ()
moveEvent direction = do
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
    maybe (return ()) (player . P.pos .=) stairsUp
environmentReactEvent (Stair Upwards) = do
    curr <- use currentLevel
    -- Move to previous level only if the player is not on the starting level
    unless (curr <= 0) $ do
        currentLevel -= 1
        stairsDown <- getCellPositionM (Stair Downwards)
        maybe (return ()) (player . P.pos .=) stairsDown
environmentReactEvent _ = return ()

{- | Given the current level, get the first position (if any) of the specified cell type.
Uses the current level in the game state.
-}
getCellPositionM :: Cell -> GameEvent (Maybe Coordinate)
getCellPositionM cell = do
    curr <- use currentLevel
    level <- use (world . to (!! curr))
    return $ getCellPosition cell level

-- | Given a cell and a level, get the first position (if any) of the specified cell type
getCellPosition :: Cell -> Level -> Maybe Coordinate
getCellPosition cell level = fst <$> find ((== cell) . snd) (assocs $ level ^. cells)

playerAttackEvent :: M.Monster -> GameEvent ()
playerAttackEvent monster = do
    me <- use player
    curr <- use currentLevel
    everyone <- use (world . to (!! curr) . monsters)
    monster' <- me `attack` monster
    me' <- monster' `attack` me

    -- Get target monster, attack it, and grab the remaining monsters
    let others = filter (/= monster) everyone
        monsterIsAlive = monster' ^. M.health > 0
        remaining =
            if not monsterIsAlive
                then others
                else monster' : others

    -- Update the list of all the monsters
    world . element curr . monsters .= remaining

    -- Update the player to after being attacked by the monster
    player .= me'

    -- If the monster is dead, move the player to the position of the deceased monster
    unless monsterIsAlive (player . P.pos .= monster' ^. M.position)
