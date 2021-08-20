{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (liftM, forM_)

import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (fromRight)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Text hiding (head, take)

import System.Random

-- https://hackage.haskell.org/package/amqp-0.22.0/docs/Network-AMQP.html
import Network.AMQP

import Data.ProtoLens (showMessage, decodeMessage)
import qualified Data.ProtoLens.Message as Proto (Message)

import qualified Proto.Search as Search

import Amqp
import Handlers.GetSearchSuggestions (routingKey, buildGetSearchSuggestionsRequest)
import Handler
import Proto
import Handlers.SubmitSearchQuery (routingKey, buildSubmitSearchQueryRequest)

randString :: Int -> IO Text
randString n = liftM (pack . take n . randomRs ('a','z')) newStdGen

randRange :: Int -> Int -> IO Int
randRange a b = liftM (head . randomRs (a, b)) newStdGen

receiveResponse :: (Proto.Message m) => String -> (BL.ByteString -> m) -> Channel -> Text -> IO ()
receiveResponse name decodeMessage channel queue =
  void $ consumeMsgs channel queue Ack handler
  where
    handler (m, e) = do
      let msg = decodeMessage (msgBody m)
      putStrLn $ "Receiving " ++ name ++ " response at " ++ unpack queue ++ ":"
      putStrLn $ showMessage msg
      ackEnv e

sendRequest :: (Proto.Message m) => String -> (Text -> m) -> Text -> Channel -> Text -> Text -> Text -> IO ()
sendRequest name builder routingKey channel replyTo correlationId q = do
  putStrLn $ "Sending " ++ name ++ " request, " ++ unpack q ++ ", to " ++ unpack replyTo ++ " with ID " ++ unpack correlationId
  let request = newMsg { msgBody = encodeProtobuf (builder q)
                       , msgCorrelationID = Just correlationId
                       , msgReplyTo = Just replyTo
                       }
  void $ publishMsg channel (exchangeName directExchange) routingKey request

receiveSearchSuggestions :: Channel -> Text -> IO ()
receiveSearchSuggestions = receiveResponse "GetSearchSuggestions" (decodeProtobuf :: BL.ByteString -> Search.GetSearchSuggestionsResponse)

receiveSearchQuery :: Channel -> Text -> IO ()
receiveSearchQuery = receiveResponse "SubmitSearchQuery" (decodeProtobuf :: BL.ByteString -> Search.SubmitSearchQueryResponse)

sendSearchSuggestions :: Channel -> Text -> Text -> Text -> IO ()
sendSearchSuggestions = sendRequest "GetSearchSuggestions" buildGetSearchSuggestionsRequest Handlers.GetSearchSuggestions.routingKey

sendSearchQuery :: Channel -> Text -> Text -> Text -> IO ()
sendSearchQuery = sendRequest "SubmitSearchQuery" buildSubmitSearchQueryRequest Handlers.SubmitSearchQuery.routingKey

sendAndReceive ::
  Channel
  -> (Channel -> Text -> IO ())
  -> (Channel -> Text -> Text -> Text -> IO ())
  -> Text
  -> IO ()
sendAndReceive channel r s q = do
  replyTo <- randString 10
  correlationId <- randString 10
  setupReplyQueue channel replyTo

  r channel replyTo
  s channel replyTo correlationId q

delay :: IO () -> IO ()
delay f = do
  ms <- randRange 500 2000
  threadDelay ms
  f

main :: IO ()
main = do
  putStrLn "Sending test requests"
  conn <- amqpConnectionFromEnv
  channel <- createChannel conn

  forM_ [1..25] $ \i -> do
    forkIO $ delay $ sendAndReceive channel receiveSearchSuggestions sendSearchSuggestions (pack $ show i)
    forkIO $ delay $ sendAndReceive channel receiveSearchQuery sendSearchQuery (pack $ show i)

  void getLine

  closeConnection conn
