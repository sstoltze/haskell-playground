{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Elasticsearch where

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor           (void)
import           Data.Text              (Text, pack)
import           Network.HTTP.Req
import           System.Environment     (lookupEnv)


data ElasticsearchIndex = ElasticsearchIndex { elasticsearchHost  :: Text
                                             , elasticsearchPort  :: Text
                                             , elasticsearchIndex :: Text
                                             }
  deriving (Show)

newtype ElasticsearchResponse = ElasticsearchResponse { elasticsearchResponseHits :: [ElasticsearchHits] }
  deriving (Show)

data ElasticsearchHits = ElasticsearchHits { elasticsearchHitQuery :: Text
                                           , elasticsearchHitScore :: Double
                                           }
  deriving (Show)

instance FromJSON ElasticsearchHits where
  parseJSON = withObject "ElasticsearchHits" $ \h -> ElasticsearchHits
    <$> either parseFail (.: "query") (parseEither (.: "_source") h)
    <*> (h .: "_score")

instance FromJSON ElasticsearchResponse where
  parseJSON = withObject "ElasticsearchResponse" $ \r -> fmap ElasticsearchResponse (either parseFail (.: "hits") (parseEither (.: "hits") r))

elasticsearchIndexFromEnv :: IO ElasticsearchIndex
elasticsearchIndexFromEnv = do
  void (loadFile defaultConfig)
  esHost <- maybe ""    pack <$> lookupEnv "AWS_ELASTICSEARCH_HOST"
  esPort <- maybe "443" pack <$> lookupEnv "AWS_ELASTICSEARCH_PORT"
  let index = "queries"
  return (ElasticsearchIndex esHost esPort index)

elasticsearchGetSuggestionsPayload :: Text -> Value
elasticsearchGetSuggestionsPayload query = object [ "query" .= object [ "match" .= object ["query" .= query] ] ]

elasticsearchSubmitQueryPayload :: Text -> Value
elasticsearchSubmitQueryPayload query = object [ "query" .= query ]

elasticsearchGetSuggestionsUrl :: ElasticsearchIndex -> Url 'Https -- Need DataKinds
elasticsearchGetSuggestionsUrl esIndex = https (elasticsearchHost esIndex) /: elasticsearchIndex esIndex /: "_search"

elasticsearchSubmitQueryUrl :: ElasticsearchIndex -> Text -> Url 'Https
elasticsearchSubmitQueryUrl esIndex q = https (elasticsearchHost esIndex) /: elasticsearchIndex esIndex /: "_doc" /: q

elasticsearchGetSuggestions :: ElasticsearchIndex -> Text -> IO ElasticsearchResponse
elasticsearchGetSuggestions esIndex query = runReq defaultHttpConfig $ do
  let payload = elasticsearchGetSuggestionsPayload query
  r <- req POST (elasticsearchGetSuggestionsUrl esIndex) (ReqBodyJson payload) jsonResponse mempty
  return (responseBody r :: ElasticsearchResponse)

elasticsearchSubmitQuery :: ElasticsearchIndex -> Text -> IO ()
elasticsearchSubmitQuery esIndex query = runReq defaultHttpConfig $ do
  let payload = elasticsearchSubmitQueryPayload query
  _ <- req POST (elasticsearchSubmitQueryUrl esIndex query) (ReqBodyJson payload) jsonResponse mempty :: Req (JsonResponse Value)
  return ()
