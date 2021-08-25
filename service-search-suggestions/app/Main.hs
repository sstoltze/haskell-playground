{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (forM_, forever)
import           Data.Functor                  (void)

import           Network.AMQP

import           Amqp
import           Handler
import           Handlers.GetSearchSuggestions (handler)
import           Handlers.SubmitSearchQuery    (handler)



main :: IO ()
main = do
  putStrLn "Create connection"
  conn <- amqpConnectionFromEnv
  putStrLn "Create channel"
  channel <- createChannel conn

  let handlers = [Handlers.GetSearchSuggestions.handler, Handlers.SubmitSearchQuery.handler]
  putStrLn "Setup handlers"

  forM_ handlers $ handlerSetupQueue channel

  putStrLn "Run handlers"
  forM_ handlers $ runHandler channel

  void getLine

  closeConnection conn
