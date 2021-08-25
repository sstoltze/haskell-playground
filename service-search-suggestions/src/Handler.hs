{-# LANGUAGE OverloadedStrings #-}
module Handler where

import           Control.Concurrent         (forkIO)
import           Control.Monad              (forever)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Functor               (void)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text, intercalate)
import           Network.AMQP

import           Amqp

data Handler = Handler { handlerRoutingKey :: Text
                       , handlerHandler    :: Message -> IO ByteString
                       }

handlerRoutingKeyPrefix :: Text
handlerRoutingKeyPrefix = "service-search-suggestions"

buildRoutingKey :: Text -> Text -> Text
buildRoutingKey version route =
  intercalate "." [handlerRoutingKeyPrefix, version, route]

handlerSetupQueue :: Channel -> Handler -> IO ()
handlerSetupQueue channel handler =
  setupHandlerQueue channel (handlerRoutingKey handler)

handleMessage :: (Message -> IO ByteString) -> (Message, Envelope) -> IO ()
handleMessage handler (m, e) = do
  -- Throw an error if no replyTo is set
  let replyTo = fromJust $ msgReplyTo m
  replyBody <- handler m
  let reply = newMsg { msgBody = replyBody
                     , msgCorrelationID = msgCorrelationID m
                     }
  let channel = envChannel e
  void $ publishMsg channel defaultExchangeName replyTo reply
  ackEnv e

runHandler :: Channel -> Handler -> IO ()
runHandler channel h = do
  let handler = handleMessage (handlerHandler h)
  let queue = handlerRoutingKey h
  void $ consumeMsgs channel queue Ack handler
