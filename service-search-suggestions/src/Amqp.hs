{-# LANGUAGE OverloadedStrings #-}

module Amqp where

import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Maybe (fromMaybe)
import Data.Functor (void)
import Data.Text
import System.Environment (lookupEnv)

import Network.AMQP

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
