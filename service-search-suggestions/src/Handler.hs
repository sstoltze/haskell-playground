module Handler where

import Data.Maybe (fromJust)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.AMQP

import Amqp

data Handler = Handler { handlerRoutingKey :: Text
                       , handlerHandler :: Message -> IO BL.ByteString
                       }

setupHandlerQueue :: Channel -> Handler -> IO ()
setupHandlerQueue channel h = do
  let handlerQueue = handlerRoutingKey h
  void $ declareQueue channel newQueue { queueName = handlerQueue
                                       , queueAutoDelete = True
                                       }
  bindQueue channel handlerQueue (exchangeName directExchange) (handlerRoutingKey h)

setupReplyQueue :: Channel -> Text -> IO ()
setupReplyQueue channel replyTo = do
  void $ declareQueue channel newQueue { queueName = replyTo
                                       , queueAutoDelete = True
                                       , queueExclusive = True
                                       }
  bindQueue channel replyTo (exchangeName directExchange) replyTo

handleMessage :: (Message -> IO BL.ByteString) -> (Message, Envelope) -> IO ()
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
  let handlerQueue = handlerRoutingKey h
  void $ consumeMsgs channel handlerQueue Ack handler
