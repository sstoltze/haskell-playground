{-# LANGUAGE OverloadedStrings #-}
module Handlers.GetSearchSuggestions where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text, pack)
import GHC.Word (Word32)

import Data.ProtoLens (defMessage, showMessage)
import Lens.Micro
import qualified Proto.Search as Search
import qualified Proto.Search_Fields as Search

import Network.AMQP

import Handler
import Protobuf

buildGetSearchSuggestionsResponse :: [Text] -> Search.GetSearchSuggestionsResponse
buildGetSearchSuggestionsResponse r =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.GetSearchSuggestionsResponse'Success
    success =
      defMessage
      & Search.result .~ r

buildGetSearchSuggestionsRequest :: Text -> Word32 -> Bool -> Search.GetSearchSuggestionsRequest
buildGetSearchSuggestionsRequest query limit isSafe =
  defMessage
  & Search.query .~ query
  & Search.limit .~ limit
  & Search.isSafe .~ isSafe

routingKey :: Text
routingKey = buildRoutingKey "v1" "get-search-suggestions"

handle :: Message -> IO ByteString
handle m = do
  let msg = decodeProtobuf (msgBody m) :: Either String Search.GetSearchSuggestionsRequest
  putStrLn "GetSearchResponse handler received:"
  putStrLn $ either ("Error: " ++) showMessage msg
  let response = \req -> (req ^. Search.query) : ["test", "af", "haskell"]
  let resp = either (buildInvalidRequestError . pack) (buildGetSearchSuggestionsResponse . response) msg
  return $ encodeProtobuf resp

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
