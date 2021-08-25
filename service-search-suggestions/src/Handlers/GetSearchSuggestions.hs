{-# LANGUAGE OverloadedStrings #-}
module Handlers.GetSearchSuggestions where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text                  (Text, pack)
import           GHC.Word                   (Word32)

import           Data.ProtoLens             (defMessage, showMessage)
import           Lens.Micro
import qualified Proto.Search               as Search
import qualified Proto.Search_Fields        as Search

import           Network.AMQP

import           Elasticsearch
import           Handler
import           Protobuf

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
  esIndex <- elasticsearchIndexFromEnv
  let buildResponse = \req -> do
        let query = req ^. Search.query
        suggestions <- elasticsearchGetSuggestions esIndex query
        let hits = elasticsearchHitQuery <$> elasticsearchResponseHits suggestions
        return $ buildGetSearchSuggestionsResponse hits
  resp <- either (return . buildInvalidRequestError . pack) buildResponse msg
  return $ encodeProtobuf resp

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
