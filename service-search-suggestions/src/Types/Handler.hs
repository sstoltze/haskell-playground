module Types.Handler (Handler(..),
                      HandlerContext(..),
                      HandlerIO) where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text                  as T
import           Network.AMQP

import           Types.Elasticsearch
import           Types.Statsd

data Handler = Handler { handlerRoutingKey :: T.Text
                       , handlerHandler    :: Message -> HandlerIO ByteString
                       }

data HandlerContext = HandlerContext { contextChannel :: Channel
                                     , contextElasticsearchIndex :: ElasticsearchIndex
                                     , contextUnsafeWords :: [T.Text]
                                     , contextCounter :: HandlerCounter
                                     }

type HandlerIO = ReaderT HandlerContext IO
