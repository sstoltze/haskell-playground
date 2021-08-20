{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Functor (void)

-- https://hackage.haskell.org/package/amqp-0.22.0/docs/Network-AMQP.html
import Network.AMQP

import Amqp
import Handler
import Handlers.GetSearchSuggestions (handler)
import Handlers.SubmitSearchQuery (handler)

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
