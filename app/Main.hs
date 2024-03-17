{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Center
import Data.List.Split
import qualified Data.Map as Map
import GHC.Arr
import Graphics.Vty
import Lens.Micro
import Lens.Micro.TH
import Player
import World

type Name = ()

data GameState = GameState
    { _player :: Player.Player
    , _world :: World
    }
    deriving (Show)

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Level

app :: App GameState () Name
app =
    App
        { appDraw = drawGame
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey (KChar 'q') [] -> halt
                -- Movement
                EvKey (KChar 'w') [] -> modify (\g -> g & player . pos %~ \(y, x) -> (y - 1, x))
                EvKey (KChar 'a') [] -> modify (\g -> g & player . pos %~ \(y, x) -> (y, x - 1))
                EvKey (KChar 's') [] -> modify (\g -> g & player . pos %~ \(y, x) -> (y + 1, x))
                EvKey (KChar 'd') [] -> modify (\g -> g & player . pos %~ \(y, x) -> (y, x + 1))
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = \_ -> attrMap defAttr []
        }

drawGame :: GameState -> [Widget Name]
drawGame game = [center $ vBox $ drawRoom game]

drawRoom :: GameState -> [Widget Name]
drawRoom game = [vBox (hBox <$> rows)]
  where
    currentRoom = head (game ^. world)
    rows = chunksOf 10 $ do
        (coord, cell) <- assocs $ currentRoom ^. cells
        let x = Map.lookup coord (currentRoom ^. monsters)
        return $
            if (game ^. player . pos) == coord
                then str $ show (game ^. player)
                else str $ maybe (show cell) show x

main :: IO ()
main = do
    let initialState = GameState (Player "Mr. Bean" (0, 0)) [emptyLevel]
    finalState <- defaultMain app initialState
    print finalState
