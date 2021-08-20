{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Data.Functor (void)
import Data.Text

-- https://hackage.haskell.org/package/amqp-0.22.0/docs/Network-AMQP.html
import Network.AMQP

import Amqp
import Handler
import qualified Handlers.GetSearchSuggestions (handler)
import qualified Handlers.SubmitSearchQuery (handler)

main :: IO ()
main = do
  putStrLn "Create connection"
  conn <- amqpConnectionFromEnv
  putStrLn "Create channel"
  channel <- createChannel conn
  putStrLn "Setup handler"
  let handlers = [Handlers.GetSearchSuggestions.handler, Handlers.SubmitSearchQuery.handler]
  forM_ handlers $ setupHandlerQueue channel

  forM_ handlers $ runHandler channel

  void getLine

  closeConnection conn
