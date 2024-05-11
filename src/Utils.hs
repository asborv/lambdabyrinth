module Utils where
import Brick (Widget)
import Brick.Widgets.Core (withAttr)
import Brick.AttrMap (attrName)
import Data.Text (Text, pack)

withMaybeAttr :: Bool -> String -> Widget n -> Widget n
withMaybeAttr asciiOnly name =
    if asciiOnly
        then withAttr $ attrName name
        else withAttr mempty

tshow :: Show a => a -> Text
tshow = pack . show
