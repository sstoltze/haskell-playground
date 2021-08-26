{-# LANGUAGE OverloadedStrings #-}
module Handler where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Functor               ((<&>))
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Network.AMQP

import           Amqp
import           Elasticsearch

data Handler = Handler { handlerRoutingKey :: T.Text
                       , handlerHandler    :: Message -> HandlerIO ByteString
                       }


data HandlerContext = HandlerContext { contextChannel :: Channel
                                     , contextElasticsearchIndex :: ElasticsearchIndex
                                     , contextUnsafeWords :: [T.Text]
                                     }

type HandlerIO = ReaderT HandlerContext IO

elasticsearchIndex :: IO ElasticsearchIndex
elasticsearchIndex = elasticsearchIndexFromEnv searchIndexName

unsafeWords :: IO [T.Text]
unsafeWords = T.IO.readFile "./src/unsafe_words.txt" <&> filter (not . T.null) . T.lines

createHandlerContext :: Channel -> IO HandlerContext
createHandlerContext channel = do
  esIndex <- elasticsearchIndex
  unsafe <- unsafeWords
  return $ HandlerContext { contextChannel = channel
                          , contextElasticsearchIndex = esIndex
                          , contextUnsafeWords = unsafe
                          }

handlerRoutingKeyPrefix :: T.Text
handlerRoutingKeyPrefix = "service-search-suggestions"

searchIndexName :: T.Text
searchIndexName = "queries"

buildRoutingKey :: T.Text -> T.Text -> T.Text
buildRoutingKey version route =
  T.intercalate "." [handlerRoutingKeyPrefix, version, route]

handlerSetupQueue :: Channel -> Handler -> IO ()
handlerSetupQueue channel handler =
  setupHandlerQueue channel (handlerRoutingKey handler)

handleMessage :: (Message -> HandlerIO ByteString) -> (Message, Envelope) -> HandlerIO ()
handleMessage handler (m, e) = do
  -- Throw an error if no replyTo is set
  let replyTo = fromJust $ msgReplyTo m
  replyBody <- handler m
  let reply = newMsg { msgBody = replyBody
                     , msgCorrelationID = msgCorrelationID m
                     }
  let channel = envChannel e
  liftIO $ void $ publishMsg channel defaultExchangeName replyTo reply
  liftIO $ ackEnv e

runHandler :: Handler -> HandlerIO ()
runHandler h = do
  context <- ask
  let channel = contextChannel context
  let handler (m, e) = runReaderT (handleMessage (handlerHandler h) (m, e)) context
  let queue = handlerRoutingKey h
  liftIO $ void $ consumeMsgs channel queue Ack handler

setupAndRunHandler :: Channel -> Handler -> IO ()
setupAndRunHandler channel handler = do
  context <- createHandlerContext channel
  handlerSetupQueue channel handler
  runReaderT (runHandler handler) context
