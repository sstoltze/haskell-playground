{-# LANGUAGE OverloadedStrings #-}
module Handlers.GetSearchSuggestions where

import Data.Text
import Data.ByteString.Lazy.Char8 (ByteString)

import Lens.Micro
import Data.ProtoLens (defMessage, showMessage)
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

import Handler
import Network.AMQP
import Protobuf (encodeProtobuf, decodeProtobufWithDefault)

buildSearchSuggestionsResponse :: [Text] -> Search.GetSearchSuggestionsResponse
buildSearchSuggestionsResponse r =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.GetSearchSuggestionsResponse'Success
    success =
      defMessage
      & Search.result .~ r

routingKey :: Text
routingKey = "service-search-suggestions.v1.get-search-suggestions"

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }

handle :: Message -> IO ByteString
handle m = do
  let msg = decodeProtobufWithDefault (msgBody m) :: Search.GetSearchSuggestionsRequest
  putStrLn "GetSearchResponse handler received:"
  putStrLn $ showMessage msg
  let response = ["test", "af", "haskell"]
  let resp = buildSearchSuggestionsResponse response
  return $ encodeProtobuf resp

buildGetSearchSuggestionsRequest :: Text -> Search.GetSearchSuggestionsRequest
buildGetSearchSuggestionsRequest query =
  defMessage
  & Search.query .~ query
  & Search.limit .~ 10
  & Search.isSafe .~ True
