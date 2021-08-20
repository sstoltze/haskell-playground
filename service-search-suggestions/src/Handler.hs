module Handler where

import Data.Maybe (fromJust)
import Data.Functor (void)
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.AMQP

import Amqp

data Handler = Handler { handlerRoutingKey :: Text
                       , handlerHandler :: Message -> IO ByteString
                       }

setupHandlerQueue :: Channel -> Handler -> IO ()
setupHandlerQueue channel h = do
  let queue = handlerRoutingKey h
  declareAndBindQueue channel (handlerQueueOpts queue) queue

setupReplyQueue :: Channel -> Text -> IO ()
setupReplyQueue channel replyTo =
  declareAndBindQueue channel (replyQueueOpts replyTo) replyTo

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
