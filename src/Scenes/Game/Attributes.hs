module Scenes.Game.Attributes where

import Brick (Widget)
import Brick.AttrMap (AttrName, attrName)
import Brick.Widgets.Core (withAttr)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

{- | Attribute names as symbols
This allows us to have type safe attributes (as long as we don't use 'withAttr' directly)
-}
data AttrNameSymbol (s :: Symbol) where
    MonsterAttr :: AttrNameSymbol "monster"
    ChestAttr :: AttrNameSymbol "chest"
    WallAttr :: AttrNameSymbol "wall"

-- | Convert an attribute symbol to an attribute name
attrNameSymbol :: KnownSymbol s => AttrNameSymbol s -> AttrName
attrNameSymbol = attrName . symbolVal

-- |  Apply an attribute by symbol to a widget
withSymbolAttr :: KnownSymbol s => AttrNameSymbol s -> Widget n -> Widget n
withSymbolAttr = withAttr . attrNameSymbol

-- | Optionally apply an attribute to a widget
withOptionalAttr :: KnownSymbol s => Bool -> AttrNameSymbol s -> Widget n -> Widget n
withOptionalAttr asciiOnly symbol =
    if asciiOnly
        then withSymbolAttr symbol
        else withAttr mempty
