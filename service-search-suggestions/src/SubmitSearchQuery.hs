{-# LANGUAGE OverloadedStrings #-}
module SubmitSearchQuery where

import Data.Text
import Data.Either (fromRight)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL

import Lens.Micro
import Data.ProtoLens (defMessage, encodeMessage, showMessage, decodeMessage)
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

import Handler
import Network.AMQP
import Proto (encodeProtobuf, decodeProtobuf)

searchQueryResponseSuccess :: Search.SubmitSearchQueryResponse
searchQueryResponseSuccess =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.SubmitSearchQueryResponse'Success
    success = defMessage

routingKey :: Text
routingKey = "service-search-suggestions.v1.submit-search-query"

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }

handle :: Message -> IO BL.ByteString
handle m = do
  let msg = decodeProtobuf (msgBody m) :: Search.SubmitSearchQueryRequest
  putStrLn "SubmitSearchQuery handler received:"
  putStrLn $ showMessage msg
  let resp = searchQueryResponseSuccess
  return $ encodeProtobuf resp

buildSubmitSearchQueryRequest :: Text -> Search.GetSearchSuggestionsRequest
buildSubmitSearchQueryRequest query =
  defMessage
  & Search.query .~ query
