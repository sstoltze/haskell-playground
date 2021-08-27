{-# LANGUAGE OverloadedStrings #-}

module Config (searchIndexName,
               handlerRoutingKeyPrefix,
               unsafeWords) where

import           Data.Functor ((<&>))
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO

searchIndexName :: T.Text
searchIndexName = "queries"

handlerRoutingKeyPrefix :: T.Text
handlerRoutingKeyPrefix = "service-search-suggestions"

unsafeWords :: IO [T.Text]
unsafeWords = T.IO.readFile "./src/unsafe_words.txt" <&> filter (not . T.null) . T.lines
