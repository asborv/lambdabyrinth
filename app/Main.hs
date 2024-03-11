{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Brick
import Brick.Widgets.Center (center)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import GHC.Arr
import Graphics.Vty

type World = [Level]
type Level = [Room]
type Name = ()
type Coordinate = (Int, Int)

data Player = Player
    { name :: String
    , pos :: Coordinate
    }

data Monster = Zombie | Ghost

data Room = Room
    { cells :: Array Coordinate Cell
    , monsters :: Map.Map Coordinate Monster
    }
    deriving (Show)

data VerticalDirection = Upwards | Downwards

data Cell = Floor | Wall | Door | Stair VerticalDirection

data GameState = GameState
    { player :: Player
    , world :: World
    }
    deriving (Show)

instance Show Cell where
    show Floor = ".."
    show Wall = "##"
    show Door = "λ "
    show (Stair Upwards) = "Λ "
    show (Stair Downwards) = "V "

instance Show Monster where
    show Zombie = "🧟"
    show Ghost = "👻"

instance Show Player where
    show _ = "😎"

app :: App GameState () Name
app =
    App
        { appDraw = drawGame
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey (KChar 'q') [] -> halt
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = \_ -> attrMap defAttr []
        }

emptyRoom :: Room
emptyRoom =
    Room
        { cells = listArray ((0, 0), (9, 9)) (replicate 100 Floor)
        , monsters = Map.fromList [((2, 2), Zombie), ((4, 5), Ghost)]
        }

drawGame :: GameState -> [Widget Name]
drawGame g = [center $ vBox $ drawRoom g]

drawRoom :: GameState -> [Widget Name]
drawRoom (GameState {world, player}) = [vBox (hBox <$> rows)]
  where
    currentRoom = head $ head world
    rows = chunksOf 10 $ do
        (coord, cell) <- assocs $ cells currentRoom
        let x = Map.lookup coord (monsters currentRoom)
        return $
            if pos player == coord
                then str $ show player
                else str $ maybe (show cell) show x

main :: IO ()
main = do
    let initialState = GameState (Player "Mr. Bean" (0, 0)) [[emptyRoom]]
    finalState <- defaultMain app initialState
    print finalState
