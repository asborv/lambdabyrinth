{- |
Module      : Scenes.Game.Events
Description : Player- or environment-triggered events in the game.
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Game.Events where

import Brick (EventM, halt)
import Config
import Control.Lens (element, to, use, (+=), (-=), (.=), (^.))
import Control.Monad (when)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.Writer (WriterT, tell)
import Creatures.Combatant
import qualified Creatures.Monsters as M
import qualified Creatures.Player as P
import Data.Foldable (find)
import Data.Text (Text, pack)
import GHC.Arr (indices, (!))
import Items.Weapons (Weapon (weaponType))
import Types
import World.Cells
import World.Level

type GameEvent a = ReaderT Config (WriterT [Text] (EventM Name GameState)) a

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
        if
            | Just m <- monster -> playerAttackEvent m
            | target == level ^. up -> player . P.pos .= target >> environmentReactEvent (Stair Upwards)
            | target == level ^. down -> player . P.pos .= target >> environmentReactEvent (Stair Downwards)
            | otherwise -> player . P.pos .= target >> environmentReactEvent cell

    me <- use player
    when (me ^. P.health <= 0) (lift $ lift halt)

tshow :: Show a => a -> Text
tshow = pack . show

{- | Modify the game state as a reaction to a player entering a cell
(1) Increment/decrement level for staircases
-}
environmentReactEvent :: Cell -> GameEvent ()
environmentReactEvent (Stair Downwards) = do
    -- Go to next level
    currentLevel += 1
    curr <- use currentLevel
    l <- use (world . to (!! curr))
    player . P.pos .= l ^. up
    tell ["You descend the stairs... Welcome to level " <> tshow curr <> "!"]
environmentReactEvent (Stair Upwards) = do
    curr <- use currentLevel
    -- Move to previous level only if the player is not on the starting level
    if curr > 0
        then do
            currentLevel -= 1
            curr' <- use currentLevel
            l <- use (world . to (!! curr'))
            player . P.pos .= l ^. down
            tell ["You cowardly retreat back to level " <> tshow curr' <> "!"]
        else tell ["Ya gotta venture down the Lambdabyrinth, ya doofus!"]
environmentReactEvent _ = return ()

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
    if monsterIsAlive
        then
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
        else do
            tell ["You slew the " <> tshow (monster ^. M.monsterType) <> "!"]
            player . P.pos .= monster' ^. M.position
