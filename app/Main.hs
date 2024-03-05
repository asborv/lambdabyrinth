{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty

placeholder :: Widget n
placeholder =
    joinBorders
        $ borderWithLabel
            (str "Hello!")
        $ withBorderStyle
            unicode
            ( center (str "Left")
                <+> vBorder
                <+> center (str "Right")
                <=> hBorder
                <=> center (str "Down")
            )

data GameState = GameState deriving (Show)

app :: App GameState () String
app =
    App
        { appDraw = const [placeholder]
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \case
            VtyEvent e -> case e of
                EvKey (KChar 'q') [] -> halt
                _ -> return ()
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = \_ -> attrMap defAttr []
        }

main :: IO ()
main = do
    let initialState = GameState
    finalState <- defaultMain app initialState
    print finalState
