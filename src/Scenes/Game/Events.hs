{- |
Module      : Scenes.Game.Events
Description : Player- or environment-triggered events in the game.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Game.Events where

import Brick (EventM, halt, zoom, modify)
import Brick.Widgets.Dialog (Dialog, dialogSelection)
import Config
import Control.Lens (Ixed (ix), to, use, (%=), (.=), (<~), (?=), (^.), at, Each (each), _3, (-=), uses)
import Control.Monad (when)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT), tell)
import Creatures.Combatant
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import GHC.Arr (indices, (!), (//))
import Items.Chest
import Items.Item
import Items.Weapon (Weapon (weaponType))
import Scenes.Game.Widgets (confirmationDialog)
import Types
import Utils
import World.Cells
import World.Level
import Utils.Zipper
import Data.List (partition)
import qualified Items.Consumable as C
import Data.Foldable (for_)

type GameEvent a name = ReaderT Config (WriterT [Text] (EventM name GameState)) a

runEvent :: Config -> GameEvent a name -> EventM name GameState a
runEvent config event = do
    (a, s) <- runWriterT $ runReaderT event config
    history %= (<> s)
    return a

{- | Check if the game is currently paused
(i.e., a dialog is open, or any other state that would block the game loop)
-}
isPaused :: GameEvent Bool name
isPaused = do
    maybeDialog <- use stairConfirmation
    let paused = isNothing maybeDialog
    return paused

{- | Move the player in the specified direction.
Accept a `Direction` and perform the necessary
checks to determine if the player can move in that direction.
If the target cell is a valid cell and is traversable,
the player's position is updated accordingly. Otherwise, nothing happens.
-}
moveEvent :: Direction -> GameEvent () name
moveEvent direction = do
    -- Get the player's position and the current level
    (y, x) <- use (player . P.position)
    level <- use (world . currentLevel)
    let levelCells = level ^. cells
        cell = levelCells ! target
        isLegalMove = target `elem` indices levelCells && isTraversible cell
        target = case direction of
            North -> (y - 1, x)
            East -> (y, x + 1)
            South -> (y + 1, x)
            West -> (y, x - 1)

    -- Update the player's position and POV only when the movement is legal
    when isLegalMove $ do
        -- Remember the cells you no longer see, and extend POV to the new position
        -- BUG FOV moves when player harms a monster, without moving
        let povUpdates = map (,Remembered) (surrounding (y, x)) <> map (,Visible) (surrounding target)
        world . currentLevel . visibility %= (// povUpdates)

        let monster = level ^. monsters . at target
        case monster of
            Just m -> playerAttackEvent m target
            Nothing -> player . P.position .= target

    -- When the player has moved, apply all gradual effects
    playerEffectsEvent

    -- If the player dies, end the game
    -- BUG env reactions like chests opening can be
    -- triggered even though there is a live monster on top of it
    me <- use player
    if me ^. P.isAlive
        then environmentReactEvent target
        else lift $ lift halt

-- | Trigger all active effects on the player
-- If the effect has run out, remove it, and inform the player
playerEffectsEvent :: GameEvent () name
playerEffectsEvent = zoom player $ do
    P.effects . each . _3 -= 1
    effects <- use P.effects

    for_ effects $ \(p, e, d) ->
        modify (P.applyEffect (C.Effect (C.Gradual d) p e))

    let (wornOff, active) = partition (\(_, _, d) -> d <= 0) effects
    P.effects .= active

    lift . tell $ map (\e -> "You feel the effects of " <> showEffect e) active
    lift . tell $ map (\e -> "The effects of " <> showEffect e <> " are wearing off...") wornOff
  where
    showEffect (p, e, _) = tshow p <> " " <> tshow e

-- | Modify the game state as a reaction to a player entering a cell
environmentReactEvent :: Coordinate -> GameEvent () name
environmentReactEvent position = do
    cell <- use (world . currentLevel . cells . to (! position))
    case cell of
        (Stair direction) -> encounterStairEvent direction
        (Chest chest) -> chestEvent chest position
        Floor -> return ()
        Wall -> return ()
        Tunnel -> return ()

{- | Event triggered when the player encounters a staircase.
If the player is going down, a confirmation dialog is shown.
If the player is going up, the player is moved to the previous level (unless they are already on the first level).
-}
encounterStairEvent :: VerticalDirection -> GameEvent () name
encounterStairEvent Downwards = stairConfirmation ?= confirmationDialog Downwards
encounterStairEvent Upwards   = use world >>= \case
    (Zipper []      _ _) -> tell ["Ya gotta venture down the Lambdabyrinth, ya doofus!"]
    (Zipper (_ : _) _ _) -> stairConfirmation ?= confirmationDialog Upwards

-- |  Event triggered when the player either confirms or cancels the stair ascent/descent.
confirmStairEvent :: Dialog VerticalDirection Bool -> GameEvent () name
confirmStairEvent d = do
    stairConfirmation .= Nothing
    case dialogSelection d of
        Just (True, direction) -> traverseStairsEvent direction
        _ -> return ()

{- | Event triggered when the player actually traverses the stairs.
This should happen after the player has encountered the stairs and confirmed the ascent/descent.
-}
traverseStairsEvent :: VerticalDirection -> GameEvent () name
traverseStairsEvent dir = do
    -- Move the level zipper up or down one level
    world  %= changeLevel

    -- Set the player's position to the stairs (up/down, depending on direction)
    player . P.position <~ use (world . currentLevel . stair)

    levelIndex <- uses (world . currentLevelIndex) tshow
    tell [mkMsg levelIndex]
  where
    (changeLevel, stair, mkMsg) = case dir of
      Upwards   -> (goLeft, down, \i -> "You cowardly retreat back to level " <> i <> "!")
      Downwards -> (goRight, up,  \i -> "You descend the stairs... Welcome to level " <> i <> "!")

{- |  Event triggered when the player walks into a chest.
 If it is already open, the player is informed.
 Otherwise, the player receives the item inside the chest.
-}
chestEvent :: Chest -> Coordinate -> GameEvent () name
chestEvent Open _ = tell ["The chest has already been opened..."]
chestEvent ((Closed contents)) position = do
    case contents of
        Nothing -> tell ["The chest is empty..."]
        Just item -> pickupEvent item
    world . currentLevel . cells . ix position .= Chest Open

{- | Represents an event of a player considering equipping an item.
Item is equipped if it is better than the current gear.
-}
pickupEvent :: BoxedItem -> GameEvent () name
pickupEvent gear = do
    player %= P.pickup gear
    tell ["You picked up a " <> tshow gear <> "!"]

{- | Event triggered when the player attacks a monster.
If the monster is dead, it is removed, and the player moves to its position.
-}
playerAttackEvent :: M.Monster -> Coordinate -> GameEvent () name
playerAttackEvent monster monsterPos = do
    me <- use player
    monster' <- me `attack` monster
    player <~ monster' `attack` me

    if monster' ^. M.isAlive
        then harmMonsterEvent monster monster' monsterPos
        else killMonsterEvent monster  monsterPos

{- | Event triggered when a monster takes damage, but does not die.
The first argument is the monster before taking damage, and the second is the monster after taking damage.
We use this to update the monster's state in the world.
-}
harmMonsterEvent :: M.Monster -> M.Monster -> Coordinate -> GameEvent () name
harmMonsterEvent monster monster' monsterPos = do
    me <- use player

    world . currentLevel . monsters %= Map.insert monsterPos monster'

    let damage = monster ^. M.health - monster' ^. M.health
        weapon = maybe "hands" (tshow . weaponType) (me ^. P.hand)
     in tell
            [ "You swung your "
                <> weapon
                <> " towards the "
                <> tshow (monster ^. M.monsterType)
                <> " and dealt "
                <> tshow damage
                <> " damage!"
            ]

{- | Event triggered when a monster is killed.
It is removed from the world, and the player moves to its position.
-}
killMonsterEvent :: M.Monster -> Coordinate -> GameEvent () name
killMonsterEvent monster monsterPos = do
    -- Remove the monster from the list of monsters in the current level
    world . currentLevel . monsters %= Map.delete monsterPos
    tell ["You slew the " <> tshow (monster ^. M.monsterType) <> "!"]
    player . P.position .= monsterPos
