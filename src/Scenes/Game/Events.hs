{- |
Module      : Scenes.Game.Events
Description : Player- or environment-triggered events in the game.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Game.Events where

import Brick (EventM, halt)
import Brick.Widgets.Dialog (Dialog, dialogSelection)
import Config
import Control.Lens (Ixed (ix), element, to, use, (%=), (+=), (-=), (.=), (<~), (?=), (^.))
import Control.Monad (when)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT), tell)
import Creatures.Combatant
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.Foldable (find)
import Data.Maybe (isNothing)
import Data.Text (Text)
import GHC.Arr (indices, (!))
import Items.Chest
import Items.Item
import Items.Weapon (Weapon (weaponType))
import Scenes.Game.Widgets (confirmationDialog)
import Types
import Utils
import World.Cells
import World.Level

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
            Just m -> playerAttackEvent m
            Nothing -> player . P.pos .= target

    -- When the player has moved, apply all gradual effects
    playerEffectsEvent

    -- If the player dies, end the game
    me <- use player
    if me ^. P.health <= 0
        then lift $ lift halt
        else environmentReactEvent target

-- | Trigger all active effects on the player
-- If the effect has run out, remove it, and inform the player
playerEffectsEvent :: GameEvent () name
playerEffectsEvent = do
    me <- use player
    let effects = me ^. P.effects
        wornOffEffects = filter (\(_, _, d) -> d == 1) effects
        showEffect (p, e, _) = tshow p <> " " <> tshow e
    player %= P.applyActiveEffects

    tell $ map (\e -> "You feel the effects of " <> showEffect e) effects
    tell $ map (\e -> "The effects of " <> showEffect e <> " are wearing off...") wornOffEffects

-- | Modify the game state as a reaction to a player entering a cell
environmentReactEvent :: Coordinate -> GameEvent () name
environmentReactEvent position = do
    curr <- use currentLevel
    cell <- use (world . to (!! curr) . cells . to (! position))
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
encounterStairEvent Upwards = do
    curr <- use currentLevel
    if curr > 0
        then stairConfirmation ?= confirmationDialog Upwards
        else tell ["Ya gotta venture down the Lambdabyrinth, ya doofus!"]

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
traverseStairsEvent Upwards = do
    -- Move to previous level only if the player is not on the starting level
    currentLevel -= 1
    curr' <- use currentLevel
    l <- use (world . to (!! curr'))
    player . P.pos .= l ^. down
    tell ["You cowardly retreat back to level " <> tshow curr' <> "!"]
traverseStairsEvent Downwards = do
    currentLevel += 1
    curr' <- use currentLevel
    l <- use (world . to (!! curr'))
    player . P.pos .= l ^. up
    tell ["You descend the stairs... Welcome to level " <> tshow curr' <> "!"]

{- |  Event triggered when the player walks into a chest.
 If it is already open, the player is informed.
 Otherwise, the player receives the item inside the chest.
-}
chestEvent :: Chest -> Coordinate -> GameEvent () name
chestEvent Open _ = tell ["The chest has already been opened..."]
chestEvent ((Closed contents)) position = do
    curr <- use currentLevel
    case contents of
        Nothing -> tell ["The chest is empty..."]
        Just item -> pickupEvent item
    world . element curr . cells . ix position .= Chest Open

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
playerAttackEvent :: M.Monster -> GameEvent () name
playerAttackEvent monster = do
    me <- use player
    monster' <- me `attack` monster
    player <~ monster' `attack` me

    let monsterIsAlive = monster' ^. M.health > 0

    if monsterIsAlive
        then harmMonsterEvent monster monster'
        else killMonsterEvent monster

{- | Event triggered when a monster takes damage, but does not die.
The first argument is the monster before taking damage, and the second is the monster after taking damage.
We use this to update the monster's state in the world.
-}
harmMonsterEvent :: M.Monster -> M.Monster -> GameEvent () name
harmMonsterEvent monster monster' = do
    curr <- use currentLevel
    me <- use player

    world . element curr . monsters %= replace monster monster'

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
killMonsterEvent :: M.Monster -> GameEvent () name
killMonsterEvent monster = do
    curr <- use currentLevel

    -- Remove the monster from the list of monsters in the current level
    world . element curr . monsters %= filter (/= monster)
    tell ["You slew the " <> tshow (monster ^. M.monsterType) <> "!"]
    player . P.pos .= monster ^. M.position
