{- |
Module      : Scenes.Scene
Description : Handy type aliases for working with Brick
Maintainer  : asbjorn.orvedal@gmail.com
-}
module Scenes.Scene where

import Brick (App)

type Name = ()
type Scene a = App a () Name
