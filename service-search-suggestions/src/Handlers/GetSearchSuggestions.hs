{-# LANGUAGE OverloadedStrings #-}

module Handlers.GetSearchSuggestions (handler) where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.ProtoLens             (showMessage)
import           Data.Text                  (Text, pack)
import           Lens.Micro
import           Network.AMQP
import qualified Proto.Search               as Search
import qualified Proto.Search_Fields        as Search

import           Elasticsearch
import           Handler
import           Protobuf
import           Statsd

routingKey :: Text
routingKey = buildRoutingKey "v1" "get-search-suggestions"

handle :: Message -> HandlerIO ByteString
handle m = do
  let msg = decodeProtobuf (msgBody m) :: Either String Search.GetSearchSuggestionsRequest
  liftIO $ putStrLn "GetSearchResponse handler received:"
  liftIO $ putStrLn $ either ("Error: " ++) showMessage msg
  resp <- either handleInvalidRequest handleResponse msg
  return $ encodeProtobuf resp

handleInvalidRequest :: String -> HandlerIO Search.GetSearchSuggestionsResponse
handleInvalidRequest s = do
  statsdHandlerFailure
  return $ buildInvalidRequestError $ pack s

handleResponse :: Search.GetSearchSuggestionsRequest -> HandlerIO Search.GetSearchSuggestionsResponse
handleResponse req = do
  esIndex <- asks contextElasticsearchIndex
  let query = req ^. Search.query
  suggestions <- liftIO $ elasticsearchGetSuggestions esIndex query
  statsdHandlerSuccess
  return $ buildGetSearchSuggestionsResponse suggestions

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
