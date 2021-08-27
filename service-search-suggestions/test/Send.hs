{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad                 (forM_, liftM)
import           Data.ByteString.Lazy          (toStrict)
import           Data.ByteString.Lazy.Char8    (ByteString)
import           Data.Either                   (fromRight)
import           Data.Functor                  (void)
import           Data.Maybe                    (fromJust)
import           Data.ProtoLens                (decodeMessage, showMessage)
import qualified Data.ProtoLens.Message        as Proto (Message)
import           Data.Text                     hiding (head, take)
import           Network.AMQP
import qualified Proto.Search                  as Search
import           System.Random

import           Amqp
import           Handler
import           Handlers.GetSearchSuggestions as GetSuggestions
import           Handlers.SubmitSearchQuery    as SubmitQuery
import           Protobuf

randString :: Int -> IO Text
randString n = fmap (pack . take n . randomRs ('a','z')) newStdGen

randRange :: Int -> Int -> IO Int
randRange a b = fmap (head . randomRs (a, b)) newStdGen

receiveResponse :: (Proto.Message m) => String -> (ByteString -> m) -> Channel -> Text -> IO ()
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
                       , msgApplicationID = Just "haskell-app"
                       }
  void $ publishMsg channel (exchangeName directExchange) routingKey request

receiveSearchSuggestions :: Channel -> Text -> IO ()
receiveSearchSuggestions =
  receiveResponse "GetSearchSuggestions" (decodeProtobufWithDefault :: ByteString -> Search.GetSearchSuggestionsResponse)

receiveSubmitQuery :: Channel -> Text -> IO ()
receiveSubmitQuery =
  receiveResponse "SubmitSearchQuery" (decodeProtobufWithDefault :: ByteString -> Search.SubmitSearchQueryResponse)

sendSearchSuggestions :: Channel -> Text -> Text -> Text -> IO ()
sendSearchSuggestions = sendRequest "GetSearchSuggestions" buildReq (handlerRoutingKey GetSuggestions.handler)
  where
    buildReq q = buildGetSearchSuggestionsRequest q 10 True

sendSubmitQuery :: Channel -> Text -> Text -> Text -> IO ()
sendSubmitQuery = sendRequest "SubmitSearchQuery" buildSubmitSearchQueryRequest (handlerRoutingKey SubmitQuery.handler)

sendAndReceive ::
  Channel
  -> (Channel -> Text -> IO ())
  -> (Channel -> Text -> Text -> Text -> IO ())
  -> Text
  -> IO ()
sendAndReceive channel receiver sender query = do
  replyTo <- randString 10
  correlationId <- randString 10
  setupReplyQueue channel replyTo

  receiver channel replyTo
  sender channel replyTo correlationId query

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
  sendAndReceive channel receiveSearchSuggestions sendSearchSuggestions "sexy"
  sendAndReceive channel receiveSubmitQuery sendSubmitQuery "sexy"

  forM_ [1..25] $ \i -> do
    forkIO $ delay $ sendAndReceive channel receiveSearchSuggestions sendSearchSuggestions (pack $ show i)
    forkIO $ delay $ sendAndReceive channel receiveSubmitQuery sendSubmitQuery (pack $ show i ++ " test " ++ show (i+1))



  void getLine

  closeConnection conn
