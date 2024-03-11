{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Brick
import Brick.Widgets.Center
import Data.List.Split
import qualified Data.Map as Map
import GHC.Arr
import Graphics.Vty
import Player
import World

type Name = ()

data GameState = GameState
    { player :: Player
    , world :: World
    }
    deriving (Show)

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
