{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad                 (forM_, forever)
import           Network.AMQP                  (closeConnection)

import           Amqp                          (amqpConnectionFromEnv,
                                                createChannel)
import           Handler                       (setupAndRunHandler)
import qualified Handlers.GetSearchSuggestions as GetSuggestions (handler)
import qualified Handlers.SubmitSearchQuery    as SubmitQuery (handler)
import           Statsd                        (statsdStoreFromEnv)

main :: IO ()
main = do
  store <- statsdStoreFromEnv

  putStrLn "Create connection"
  conn <- amqpConnectionFromEnv
  putStrLn "Create channel"
  channel <- createChannel conn

  let handlers = [GetSuggestions.handler, SubmitQuery.handler]

  putStrLn "Run handlers"
  forM_ handlers $ setupAndRunHandler channel store

  forever getLine

  closeConnection conn
