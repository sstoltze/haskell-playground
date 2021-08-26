module Main (main) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (forM_, forever)
import           Data.Functor                  (void)
import           Network.AMQP                  (closeConnection)

import           Amqp                          (amqpConnectionFromEnv,
                                                createChannel)
import           Handler                       (setupAndRunHandler)
import           Handlers.GetSearchSuggestions (handler)
import           Handlers.SubmitSearchQuery    (handler)

main :: IO ()
main = do
  putStrLn "Create connection"
  conn <- amqpConnectionFromEnv
  putStrLn "Create channel"
  channel <- createChannel conn

  let handlers = [Handlers.GetSearchSuggestions.handler, Handlers.SubmitSearchQuery.handler]

  putStrLn "Run handlers"
  forM_ handlers $ setupAndRunHandler channel

  forever getLine

  closeConnection conn
