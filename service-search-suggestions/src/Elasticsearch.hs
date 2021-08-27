{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Elasticsearch (ElasticsearchIndex,
                      elasticsearchIndex,
                      elasticsearchIndexFromEnv,
                      elasticsearchGetSuggestions,
                      elasticsearchSubmitQuery) where

import           Configuration.Dotenv (defaultConfig, loadFile)
import           Data.Aeson
import           Data.Functor         (void)
import qualified Data.Text            as T (Text, pack)
import           Network.HTTP.Req
import           System.Environment   (lookupEnv)

import           Config               (searchIndexName)
import           Types.Elasticsearch

elasticsearchIndex :: IO ElasticsearchIndex
elasticsearchIndex = elasticsearchIndexFromEnv searchIndexName

elasticsearchIndexFromEnv :: T.Text -> IO ElasticsearchIndex
elasticsearchIndexFromEnv indexName = do
  void (loadFile defaultConfig)
  esHost <- maybe ""    T.pack <$> lookupEnv "AWS_ELASTICSEARCH_HOST"
  esPort <- maybe "443" T.pack <$> lookupEnv "AWS_ELASTICSEARCH_PORT"
  return $ ElasticsearchIndex esHost esPort indexName

elasticsearchGetSuggestionsPayload :: T.Text -> Value
elasticsearchGetSuggestionsPayload query = object [ "query" .= object [ "match" .= object ["query" .= query] ] ]

elasticsearchSubmitQueryPayload :: T.Text -> Value
elasticsearchSubmitQueryPayload query = object [ "query" .= query ]

elasticsearchGetSuggestionsUrl :: ElasticsearchIndex -> Url 'Https -- Need DataKinds
elasticsearchGetSuggestionsUrl esIndex = https (elasticsearchHost esIndex) /: elasticsearchIndexName esIndex /: "_search"

elasticsearchSubmitQueryUrl :: ElasticsearchIndex -> T.Text -> Url 'Https
elasticsearchSubmitQueryUrl esIndex q = https (elasticsearchHost esIndex) /: elasticsearchIndexName esIndex /: "_doc" /: q

elasticsearchGetSuggestions :: ElasticsearchIndex -> T.Text -> IO [T.Text]
elasticsearchGetSuggestions esIndex query = runReq defaultHttpConfig $ do
  let payload = elasticsearchGetSuggestionsPayload query
  r <- req POST (elasticsearchGetSuggestionsUrl esIndex) (ReqBodyJson payload) jsonResponse mempty
  let body = responseBody r :: ElasticsearchResponse
  let suggestions = elasticsearchHitQuery <$> elasticsearchResponseHits body
  return suggestions

elasticsearchSubmitQuery :: ElasticsearchIndex -> T.Text -> IO ()
elasticsearchSubmitQuery esIndex query = runReq defaultHttpConfig $ do
  let payload = elasticsearchSubmitQueryPayload query
  _ <- req POST (elasticsearchSubmitQueryUrl esIndex query) (ReqBodyJson payload) jsonResponse mempty :: Req (JsonResponse Value)
  return ()
