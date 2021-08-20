{-# LANGUAGE OverloadedStrings #-}
module Handlers.GetSearchSuggestions where

import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)

import Lens.Micro
import Data.ProtoLens (defMessage, showMessage)
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

import Network.AMQP

import Handler
import Protobuf (encodeProtobuf, decodeProtobufWithDefault)

buildGetSearchSuggestionsResponse :: [Text] -> Search.GetSearchSuggestionsResponse
buildGetSearchSuggestionsResponse r =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.GetSearchSuggestionsResponse'Success
    success =
      defMessage
      & Search.result .~ r

buildGetSearchSuggestionsRequest :: Text -> Search.GetSearchSuggestionsRequest
buildGetSearchSuggestionsRequest query =
  defMessage
  & Search.query .~ query
  & Search.limit .~ 10
  & Search.isSafe .~ True

routingKey :: Text
routingKey = buildRoutingKey "v1" "get-search-suggestions"

handle :: Message -> IO ByteString
handle m = do
  let msg = decodeProtobufWithDefault (msgBody m) :: Search.GetSearchSuggestionsRequest
  putStrLn "GetSearchResponse handler received:"
  putStrLn $ showMessage msg
  let response = ["test", "af", "haskell"]
  let resp = buildGetSearchSuggestionsResponse response
  return $ encodeProtobuf resp

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
