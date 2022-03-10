{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Amqp
import           Client
import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Concurrent
import           Control.Monad
-- import           Data.Maybe           (fromJust)
-- import qualified Data.Text            as T
import           Generate
import qualified Proto.Test           as Test
-- import           System.Environment   (lookupEnv)
import           Results
import           Util

numberOfSenders :: Int
numberOfSenders = 1

main :: IO ()
main = do
  void (loadFile defaultConfig)
  conn <- amqpConnectionFromEnv
  channel <- createChannel conn
  results <- newResults
  putStrLn "ready"
  callThreads <- forM [1 .. numberOfSenders] $
    const $
      forkIO $ do
        client <- makeRpcClient channel
        forever $ do
          request <- generateTestRequest
          let routingKey = "test-routing-key"
          reply <- call client routingKey request :: IO (Either String Test.TestResponse)
          putResult results reply
  printThread <- forkIO $
    forever $ do
      threadDelay oneSecond
      printResults results
  let threads = printThread : callThreads
  installKillHandler threads
  void getLine
  mapM_ killThread threads
