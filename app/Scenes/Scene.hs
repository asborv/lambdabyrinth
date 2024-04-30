module Scenes.Scene where

import Brick (App)

type Name = ()
type Scene a = App a () Name
newtype Config = Config {ascii :: Bool}