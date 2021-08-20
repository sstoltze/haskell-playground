{-# LANGUAGE OverloadedStrings #-}
module Handlers.SubmitSearchQuery where

import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)

import Lens.Micro
import Data.ProtoLens (defMessage, showMessage)
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

import Handler
import Network.AMQP
import Protobuf (encodeProtobuf, decodeProtobufWithDefault)

buildSubmitSearchQueryResponse :: Search.SubmitSearchQueryResponse
buildSubmitSearchQueryResponse =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.SubmitSearchQueryResponse'Success
    success = defMessage

buildSubmitSearchQueryRequest :: Text -> Search.GetSearchSuggestionsRequest
buildSubmitSearchQueryRequest query =
  defMessage
  & Search.query .~ query

routingKey :: Text
routingKey = buildRoutingKey "v1" "submit-search-query"

handle :: Message -> IO ByteString
handle m = do
  let msg = decodeProtobufWithDefault (msgBody m) :: Search.SubmitSearchQueryRequest
  putStrLn "SubmitSearchQuery handler received:"
  putStrLn $ showMessage msg
  let resp = buildSubmitSearchQueryResponse
  return $ encodeProtobuf resp

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
