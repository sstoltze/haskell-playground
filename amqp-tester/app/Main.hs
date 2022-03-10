{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Amqp
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Functor               (void)
import           Data.ProtoLens             (defMessage, showMessage)
import qualified Data.ProtoLens.Message     as Proto (Message)
import           Data.Text                  hiding (head, take)
import           GHC.Word                   (Word64)
import           Lens.Micro
import           Network.AMQP
import qualified Proto.Test                 as Test
import qualified Proto.Test_Fields          as Test
import           Protobuf
import           System.Random

receiveResponse :: (Proto.Message m) => String -> (ByteString -> m) -> Channel -> Text -> IO ()
receiveResponse name decode channel queue =
  void $ consumeMsgs channel queue Ack handler
  where
    handler (m, e) = do
      let msg = decode (msgBody m)
      putStrLn $ "Receiving " ++ name ++ " response at " ++ unpack queue ++ ":"
      putStrLn $ showMessage msg
      ackEnv e

sendRequest :: (Proto.Message m, Show a) => String -> (a -> m) -> Text -> Channel -> Text -> Text -> a -> IO ()
sendRequest name builder routingKey channel replyTo correlationId q = do
  putStrLn $ "Sending " ++ name ++ " request, " ++ show q ++ ", to " ++ unpack replyTo ++ " with ID " ++ unpack correlationId
  let request =
        newMsg
          { msgBody = encodeProtobuf (builder q),
            msgCorrelationID = Just correlationId,
            msgReplyTo = Just replyTo,
            msgApplicationID = Just "haskell-app",
            msgContentType = Just "application/octet-stream"
          }
  void $ publishMsg channel (exchangeName directExchange) routingKey request

sendAndReceive ::
  Channel ->
  (Channel -> Text -> IO ()) ->
  (Channel -> Text -> Text -> a -> IO ()) ->
  a ->
  IO ()
sendAndReceive channel receiver sender query = do
  replyTo <- randString 10
  correlationId <- randString 10
  setupReplyQueue channel replyTo

  receiver channel replyTo
  sender channel replyTo correlationId query

main :: IO ()
main = do
  putStrLn "Sending test requests"
  conn <- amqpConnectionFromEnv
  channel <- createChannel conn
  sendAndReceive
    channel
    receiver
    sender
    body

  void getLine

  closeConnection conn
  where
    body = 950442 :: Word64
    receiver = receiveResponse "TestResponse" (decodeProtobufWithDefault :: ByteString -> Test.TestResponse)
    sender =
      sendRequest
        "TestRequest"
        ((\q -> defMessage & Test.input .~ q) :: Word64 -> Test.TestRequest)
        "test-routing-key"

randString :: Int -> IO Text
randString n = fmap (pack . take n . randomRs ('a', 'z')) newStdGen
