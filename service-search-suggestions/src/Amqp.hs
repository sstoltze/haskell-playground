{-# LANGUAGE OverloadedStrings #-}

module Amqp where

import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import System.Environment (lookupEnv)

import Network.AMQP

replyQueueOpts :: Text -> QueueOpts
replyQueueOpts queue = newQueue { queueName = queue
                                , queueAutoDelete = True
                                , queueExclusive = True
                                }

handlerQueueOpts :: Text -> QueueOpts
handlerQueueOpts queue = newQueue { queueName = queue
                                  , queueAutoDelete = True
                                  }

declareAndBindQueue :: Channel -> QueueOpts -> Text -> IO ()
declareAndBindQueue channel opts routingKey = do
  void $ declareQueue channel opts
  bindQueue channel (queueName opts) (exchangeName directExchange) routingKey

setupHandlerQueue :: Channel -> Text -> IO ()
setupHandlerQueue channel queue =
  declareAndBindQueue channel (handlerQueueOpts queue) queue

setupReplyQueue :: Channel -> Text -> IO ()
setupReplyQueue channel replyTo =
  declareAndBindQueue channel (replyQueueOpts replyTo) replyTo

amqpConnectionFromEnv :: IO Connection
amqpConnectionFromEnv = do
  void (loadFile defaultConfig)
  host <- fromMaybe "amqp.tissuu.com" <$> lookupEnv "AMQP_HOST"
  user <- maybe "guest" pack          <$> lookupEnv "AMQP_USERNAME"
  pass <- maybe "guest" pack          <$> lookupEnv "AMQP_PASSWORD"
  openConnection host "/" user pass

directExchange :: ExchangeOpts
directExchange = newExchange { exchangeName = "amq.direct"
                             , exchangeType = "direct"
                             , exchangeDurable = True
                             }

defaultExchangeName :: Text
defaultExchangeName = ""

createChannel :: Connection -> IO Channel
createChannel conn = do
  channel <- openChannel conn
  qos channel 0 1 False
  declareExchange channel directExchange
  return channel
