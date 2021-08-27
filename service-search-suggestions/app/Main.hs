{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent              (forkIO)
import           Control.Monad                   (forM_, forever)
import           Data.Functor                    (void)
import           Network.AMQP                    (closeConnection)
import           System.Metrics
import           System.Remote.Monitoring.Statsd

import           Amqp                            (amqpConnectionFromEnv,
                                                  createChannel)
import           Handler                         (setupAndRunHandler)
import           Handlers.GetSearchSuggestions   (handler)
import           Handlers.SubmitSearchQuery      (handler)
import           Statsd


main :: IO ()
main = do
  let statsdClient = defaultStatsdOptions { host = "statsd.tissuu.com"
                                          , prefix = "sst.haskell-app"
                                          }
  store <- setupStatsdStore statsdClient

  putStrLn "Create connection"
  conn <- amqpConnectionFromEnv
  putStrLn "Create channel"
  channel <- createChannel conn

  let handlers = [Handlers.GetSearchSuggestions.handler, Handlers.SubmitSearchQuery.handler]

  putStrLn "Run handlers"
  forM_ handlers $ setupAndRunHandler channel store

  forever getLine

  closeConnection conn
