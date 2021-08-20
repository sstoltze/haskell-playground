{-# LANGUAGE OverloadedStrings #-}

module Handlers.SubmitSearchQuery where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text, pack)

import Data.ProtoLens (defMessage, showMessage)
import Lens.Micro
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

import Network.AMQP

import Handler
import Protobuf

buildSubmitSearchQueryResponse :: Search.SubmitSearchQueryResponse
buildSubmitSearchQueryResponse =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.SubmitSearchQueryResponse'Success
    success = defMessage

buildSubmitSearchQueryRequest :: Text -> Search.SubmitSearchQueryRequest
buildSubmitSearchQueryRequest query =
  defMessage
  & Search.query .~ query

routingKey :: Text
routingKey = buildRoutingKey "v1" "submit-search-query"

handle :: Message -> IO ByteString
handle m = do
  let msg = decodeProtobuf (msgBody m) :: Either String Search.SubmitSearchQueryRequest
  putStrLn "SubmitSearchQuery handler received:"
  putStrLn $ either ("Error: " ++) showMessage msg
  let resp = either (buildInvalidRequestError . pack) (const buildSubmitSearchQueryResponse) msg
  return $ encodeProtobuf resp

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
