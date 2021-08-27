{-# LANGUAGE OverloadedStrings #-}

module Types.Elasticsearch (ElasticsearchIndex(..),
                            ElasticsearchResponse(..),
                            ElasticsearchHits(..)) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T

data ElasticsearchIndex = ElasticsearchIndex { elasticsearchHost      :: T.Text
                                             , elasticsearchPort      :: T.Text
                                             , elasticsearchIndexName :: T.Text
                                             }
  deriving (Show)

newtype ElasticsearchResponse = ElasticsearchResponse { elasticsearchResponseHits :: [ElasticsearchHits] }


newtype ElasticsearchHits = ElasticsearchHits { elasticsearchHitQuery :: T.Text }

instance FromJSON ElasticsearchHits where
  parseJSON = withObject "ElasticsearchHits" $ \h -> ElasticsearchHits
    <$> either parseFail (.: "query") (parseEither (.: "_source") h)

instance FromJSON ElasticsearchResponse where
  parseJSON = withObject "ElasticsearchResponse" $ \r -> fmap ElasticsearchResponse (either parseFail (.: "hits") (parseEither (.: "hits") r))
