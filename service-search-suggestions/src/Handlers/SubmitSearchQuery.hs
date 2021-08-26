{-# LANGUAGE OverloadedStrings #-}

module Handlers.SubmitSearchQuery where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.ProtoLens             (defMessage, showMessage)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Lens.Micro
import           Network.AMQP
import qualified Proto.Search               as Search
import qualified Proto.Search_Fields        as Search
import           System.IO.Unsafe           (unsafeDupablePerformIO)

import           Elasticsearch
import           Handler
import           Protobuf

buildSubmitSearchQueryResponse :: Search.SubmitSearchQueryResponse
buildSubmitSearchQueryResponse =
  defMessage
  & (Search.maybe'success ?~ success)
  where
    success :: Search.SubmitSearchQueryResponse'Success
    success = defMessage

buildSubmitSearchQueryRequest :: T.Text -> Search.SubmitSearchQueryRequest
buildSubmitSearchQueryRequest query =
  defMessage
  & Search.query .~ query

routingKey :: T.Text
routingKey = buildRoutingKey "v1" "submit-search-query"

unsafePrefixes :: [T.Text]
unsafePrefixes = ["ero", "fuck", "gay", "naked", "nude", "penis", "porn", "pussy", "sex", "xx"]

-- Don't use unsafePerformIO >:(
-- Seems safe and more performant here since we don't change the file during runtime and just need to read it once
unsafeWords :: [T.Text]
unsafeWords = filter (not . T.null) . T.lines $ unsafeDupablePerformIO $ T.IO.readFile "./src/Handlers/unsafe_words.txt"

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
  context <- ask
  let msg = decodeProtobuf (msgBody m) :: Either String Search.SubmitSearchQueryRequest
  liftIO $ putStrLn "SubmitSearchQuery handler received:"
  liftIO $ putStrLn $ either ("Error: " ++) showMessage msg
  let buildResponse = \req -> do
        let query = req ^. Search.query
        when (isSafeQuery (contextUnsafeWords context) query) $
          elasticsearchSubmitQuery (contextElasticsearchIndex context) query
        return buildSubmitSearchQueryResponse
  resp <- liftIO $ either (return . buildInvalidRequestError . T.pack) buildResponse msg
  return $ encodeProtobuf resp

handler :: Handler
handler = Handler { handlerRoutingKey = routingKey
                  , handlerHandler = handle
                  }
