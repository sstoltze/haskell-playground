module App where

import Data.Text (Text, pack)

listOptions :: [Text]
listOptions = map (pack . (show :: Int -> String)) [1..10]
