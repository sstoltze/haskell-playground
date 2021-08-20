{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Functor (void)
import Data.Text

-- https://hackage.haskell.org/package/amqp-0.22.0/docs/Network-AMQP.html
import Network.AMQP

import Amqp
import Handler
import qualified GetSearchSuggestions (handler)
import qualified SubmitSearchQuery (handler)

main :: IO ()
main = do
  putStrLn "Create connection"
  conn <- amqpConnectionFromEnv
  putStrLn "Create channel"
  channel <- createChannel conn
  putStrLn "Setup handler"
  setupHandlerQueue channel GetSearchSuggestions.handler
  setupHandlerQueue channel SubmitSearchQuery.handler

  runHandler channel GetSearchSuggestions.handler
  runHandler channel SubmitSearchQuery.handler

  void getLine

  closeConnection conn
