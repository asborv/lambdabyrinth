module Scenes.Game.Widgets where
import World.Cells (VerticalDirection(..))
import Brick.Widgets.Dialog
import Brick (str)

confirmationDialog :: VerticalDirection -> Dialog VerticalDirection Bool
confirmationDialog dir =
    dialog
        (Just . str $ "Do you want to " <> action <> " the stairs?")
        (Just (True, options))
        maxWidth
  where
    action = if dir == Upwards then "ascend" else "descend"
    options = [("Yes", True, dir), ("No", False, dir)]
    maxWidth = 50
