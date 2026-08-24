module Scenes.Game.Attributes
    ( AttrNameSymbol(..)
    , attrNameSymbol
    , withAttrSymbol
    ) where

import Brick (Widget)
import Brick.AttrMap (AttrName, attrName)
import Brick.Widgets.Core (withAttr)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

{- | Attribute names as symbols
This allows us to have type safe attributes (as long as we don't use 'withAttr' directly)
-}
data AttrNameSymbol (s :: Symbol) where
    ChestAttr          :: AttrNameSymbol "chest"
    LowHealthAttr      :: AttrNameSymbol "lowHealth"
    MediumHealthAttr   :: AttrNameSymbol "mediumHealth"
    HighHealthAttr     :: AttrNameSymbol "highHealth"
    MonsterAttr        :: AttrNameSymbol "monster"
    RememberedCellAttr :: AttrNameSymbol "rememberedCell"
    WallAttr           :: AttrNameSymbol "wall"

-- | Convert an attribute symbol to an attribute name
attrNameSymbol :: KnownSymbol s => AttrNameSymbol s -> AttrName
attrNameSymbol = attrName . symbolVal

-- |  Apply an attribute by symbol to a widget
withAttrSymbol :: KnownSymbol s => AttrNameSymbol s -> Widget n -> Widget n
withAttrSymbol = withAttr . attrNameSymbol
