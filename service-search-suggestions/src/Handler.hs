{-# LANGUAGE OverloadedStrings #-}
module Handler (Handler(..),
                HandlerContext(..),
                HandlerIO,
                handlerRoutingKeyPrefix,
                createHandlerContext,
                buildRoutingKey,
                setupAndRunHandler) where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           Network.AMQP
import           System.Metrics

import           Amqp
import           Config
import           Elasticsearch
import           Statsd
import           Types.Handler

createHandlerContext :: Channel -> HandlerCounter -> IO HandlerContext
createHandlerContext channel counter = do
  esIndex <- elasticsearchIndex
  unsafe <- unsafeWords
  return $ HandlerContext { contextChannel = channel
                          , contextElasticsearchIndex = esIndex
                          , contextUnsafeWords = unsafe
                          , contextCounter = counter
                          }

buildRoutingKey :: T.Text -> T.Text -> T.Text
buildRoutingKey version route =
  T.intercalate "." [handlerRoutingKeyPrefix, version, route]

handlerSetupQueue :: Channel -> Handler -> IO ()
handlerSetupQueue channel handler =
  setupHandlerQueue channel (handlerRoutingKey handler)

handleMessage :: (Message -> HandlerIO ByteString) -> (Message, Envelope) -> HandlerIO ()
handleMessage handler (m, e) = do
  -- Throw an error if no replyTo is set
  statsdHandlerRequest
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

setupAndRunHandler :: Channel -> Store -> Handler -> IO ()
setupAndRunHandler channel store handler = do
  handlerCounter <- createHandlerCounter store (handlerRoutingKey handler)
  context <- createHandlerContext channel handlerCounter
  handlerSetupQueue channel handler
  runReaderT (runHandler handler) context
