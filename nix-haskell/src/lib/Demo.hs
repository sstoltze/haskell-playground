module Demo (hello, world) where

import Control.Lens

tuple :: (String, String)
tuple = ("Hello", "world")

hello :: String
hello = tuple^._1

world :: String
world = tuple^._2
