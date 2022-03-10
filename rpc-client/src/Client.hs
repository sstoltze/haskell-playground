{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Amqp
import           Control.Concurrent
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Functor               (void)
import qualified Data.Map                   as Map
import           Data.ProtoLens             (decodeMessage, encodeMessage)
import           Data.ProtoLens.Message     (Message)
import qualified Data.Text                  as T
import qualified Network.AMQP               as AMQP
import           System.Random

data RpcClient = RpcClient
  { rpcChannel    :: AMQP.Channel,
    rpcRoutingKey :: T.Text,
    rpcStorage    :: MVar (Map.Map T.Text (MVar ByteString))
  }

runConsumer :: RpcClient -> IO ()
runConsumer
  RpcClient
    { rpcChannel = channel,
      rpcRoutingKey = routingKey,
      rpcStorage = hashMap
    } =
    void $ AMQP.consumeMsgs channel routingKey AMQP.Ack putInHash
    where
      storeMessageBody cId val h = case Map.lookup cId h of
        Nothing -> undefined -- This should not happen
        Just var -> do
          putMVar var val
          return h
      putInHash (m, _) =
        case AMQP.msgCorrelationID m of
          Nothing -> return ()
          Just cId ->
            modifyMVar_ hashMap (storeMessageBody cId $ AMQP.msgBody m)

randString :: Int -> IO T.Text
randString n = fmap (T.pack . take n . randomRs ('a', 'z')) newStdGen

makeRpcClient :: AMQP.Channel -> IO RpcClient
makeRpcClient channel = do
  replyTo <- randString 32
  storage <- newMVar Map.empty
  setupReplyQueue channel replyTo
  let client =
        RpcClient
          { rpcChannel = channel,
            rpcRoutingKey = replyTo,
            rpcStorage = storage
          }
  runConsumer client
  return client

encodeProtobuf :: (Message m) => m -> ByteString
encodeProtobuf = fromStrict . encodeMessage

decodeProtobuf :: (Message m) => ByteString -> Either String m
decodeProtobuf b = decodeMessage $ toStrict b

prepareStorage :: RpcClient -> T.Text -> IO (MVar ByteString)
prepareStorage RpcClient {rpcStorage = storage} correlationId = modifyMVar storage $ \h -> do
  v <- newEmptyMVar
  return (Map.insert correlationId v h, v)

call :: (Message m, Message r) => RpcClient -> T.Text -> m -> IO (Either String r)
call client routingKey message = do
  correlationId <- randString 32
  v <- prepareStorage client correlationId
  let request =
        AMQP.newMsg
          { AMQP.msgBody = encodeProtobuf message,
            AMQP.msgCorrelationID = Just correlationId,
            AMQP.msgReplyTo = Just (rpcRoutingKey client),
            AMQP.msgApplicationID = Just "haskell-rpc-client",
            AMQP.msgContentType = Just "application/octet-stream"
          }
  void $ AMQP.publishMsg (rpcChannel client) (AMQP.exchangeName directExchange) routingKey request
  reply <- readMVar v
  return $ decodeProtobuf reply
