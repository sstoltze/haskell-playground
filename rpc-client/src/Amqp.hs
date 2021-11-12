{-# LANGUAGE OverloadedStrings #-}

module Amqp
  ( amqpConnectionFromEnv,
    createChannel,
    defaultExchangeName,
    directExchange,
    setupHandlerQueue,
    setupReplyQueue,
  )
where

import           Data.Functor       (void)
import           Data.Maybe         (fromJust)
import qualified Data.Text          as T (Text, pack)
import           Network.AMQP
import           System.Environment (lookupEnv)

replyQueueOpts :: T.Text -> QueueOpts
replyQueueOpts queue =
  newQueue
    { queueName = queue,
      queueAutoDelete = True,
      queueExclusive = True
    }

handlerQueueOpts :: T.Text -> QueueOpts
handlerQueueOpts queue =
  newQueue
    { queueName = queue,
      queueAutoDelete = True,
      queueDurable = False
    }

declareAndBindQueue :: Channel -> QueueOpts -> T.Text -> IO ()
declareAndBindQueue channel opts routingKey = do
  void $ declareQueue channel opts
  bindQueue channel (queueName opts) (exchangeName directExchange) routingKey

setupHandlerQueue :: Channel -> T.Text -> IO ()
setupHandlerQueue channel queue =
  declareAndBindQueue channel (handlerQueueOpts queue) queue

setupReplyQueue :: Channel -> T.Text -> IO ()
setupReplyQueue channel replyTo =
  declareAndBindQueue channel (replyQueueOpts replyTo) replyTo

amqpConnectionFromEnv :: IO Connection
amqpConnectionFromEnv = do
  host <- fromJust <$> lookupEnv "AMQP_HOST"
  user <- maybe "guest" T.pack <$> lookupEnv "AMQP_USERNAME"
  pass <- maybe "guest" T.pack <$> lookupEnv "AMQP_PASSWORD"
  openConnection host "/" user pass

directExchange :: ExchangeOpts
directExchange =
  newExchange
    { exchangeName = "amq.direct",
      exchangeType = "direct",
      exchangeDurable = True
    }

defaultExchangeName :: T.Text
defaultExchangeName = ""

createChannel :: Connection -> IO Channel
createChannel conn = do
  channel <- openChannel conn
  qos channel 0 1 False
  declareExchange channel directExchange
  return channel
