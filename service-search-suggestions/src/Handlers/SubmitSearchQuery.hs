{-# LANGUAGE OverloadedStrings #-}

module Handlers.SubmitSearchQuery (handler) where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.ProtoLens             (showMessage)
import qualified Data.Text                  as T
import           Lens.Micro
import           Network.AMQP
import qualified Proto.Search               as Search
import qualified Proto.Search_Fields        as Search

import           Elasticsearch
import           Handler
import           Protobuf
import           Statsd

routingKey :: T.Text
routingKey = buildRoutingKey "v1" "submit-search-query"

unsafePrefixes :: [T.Text]
unsafePrefixes = ["ero", "fuck", "gay", "naked", "nude", "penis", "porn", "pussy", "sex", "xx"]

isSafeQuery :: [T.Text] -> T.Text -> Bool
isSafeQuery unsafe q = all isSafeSentence subQueries
  where
    subQueries = fmap T.unwords $ orderedSubsets [[]] $ T.words q
    orderedSubsets subsets [] = drop 1 $ fmap reverse subsets
    orderedSubsets subsets (x:xs) = orderedSubsets (subsets ++ fmap (x:) subsets) xs
    isSafeSentence s = isSafePrefix s && isSafePlural s && isSafe s
    -- Check s does not contain an unsafe prefix
    isSafePrefix s = not $ any (`T.isPrefixOf` s) unsafePrefixes
    -- If s ends in 's' and is longer than six characters, strip the s and check it is still safe
    isSafePlural s = T.length s < 6 || maybe True isSafe (T.stripSuffix "s" s)
    -- s is safe if it is not an element of the list of unsafe words
    isSafe s = s `notElem` unsafe

handle :: Message -> HandlerIO ByteString
handle m = do
  let msg = decodeProtobuf (msgBody m) :: Either String Search.SubmitSearchQueryRequest
  liftIO $ putStrLn "SubmitSearchQuery handler received:"
  liftIO $ putStrLn $ either ("Error: " ++) showMessage msg
  resp <- either handleInvalidRequest handleResponse msg
  return $ encodeProtobuf resp

handleInvalidRequest :: String -> HandlerIO Search.SubmitSearchQueryResponse
handleInvalidRequest s = do
  statsdHandlerFailure
  return $ buildInvalidRequestError $ T.pack s

handleResponse :: Search.SubmitSearchQueryRequest -> HandlerIO Search.SubmitSearchQueryResponse
handleResponse req = do
  context <- ask
  let query = req ^. Search.query
  when (isSafeQuery (contextUnsafeWords context) query) $
    liftIO $ elasticsearchSubmitQuery (contextElasticsearchIndex context) query
  statsdHandlerSuccess
  return buildSubmitSearchQueryResponse

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
