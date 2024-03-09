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

class Entity a where
    position :: a -> Coordinate
    draw :: a -> Widget n

type World = [Room]
type Name = ()
data Player = Player
    { name :: String
    , pos :: Coordinate
    }
    deriving (Show)
data Room = Room
    { cells :: Array Coordinate Cell
    , entities :: Map.Map Coordinate Player
    }

instance Entity Player where
    position (Player _ pos) = pos
    draw _ = str "😎"

type Coordinate = (Int, Int)

data Cell = Floor | Wall

instance Show Cell where
    show Floor = ".."
    show Wall = "##"

data GameState = GameState
    { player :: Player
    , world :: World
    }

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
        , entities = Map.empty
        }

drawGame :: GameState -> [Widget Name]
drawGame g@(GameState {world}) = [center $ vBox $ drawRoom g]

drawRoom :: GameState -> [Widget Name]
drawRoom g@(GameState {world, player}) = [vBox (hBox <$> rows)]
  where
    rows = chunksOf 10 $ do
        (coord, cell) <- assocs $ cells $ head world
        let ents = entities $ head world
            x = Map.lookup coord ents
        return $
            if pos player == coord
                then draw player
                else case x of
                    Just p -> str (show p)
                    Nothing -> str (show cell)

-- drawRoom Room {cells, entities} = [vBox (hBox <$> rows)]
--   where
--     rows = chunksOf 10 $ elems (str . show <$> cells)

main :: IO ()
main = do
    let initialState = GameState (Player "Mr. Bean" (0, 0)) [emptyRoom]
    finalState <- defaultMain app initialState
    print "hei"
