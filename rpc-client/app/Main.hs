{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Concurrent
import           Control.Monad
import           Data.Maybe           (fromJust)
import           Data.ProtoLens       (defMessage, showMessage)
import qualified Data.Text            as T
import           System.Environment   (lookupEnv)
import           Test.QuickCheck

import qualified Proto.Test           as Test

import           Amqp
import           Client
import           Generate
import           Util

numberOfSenders :: Int
numberOfSenders = 10

main :: IO ()
main = do
  void (loadFile defaultConfig)
  conn <- amqpConnectionFromEnv
  channel <- createChannel conn
  client <- makeRpcClient channel
  results <- newResults
  putStrLn "ready"
  callThreads <- forM [1..numberOfSenders] $ const $ forkIO $ forever $ do
    request <- buildTestRequest
    routingKey <- T.pack . fromJust <$> lookupEnv "ROUTING_KEY"
    reply <- call client routingKey request :: IO (Either String Test.TestResponse)
    putResult results reply
  printThread <- forkIO $ forever $ do
    threadDelay oneSecond
    printResults results
  let threads = printThread : callThreads
  installKillHandler threads
  void getLine
  mapM_ killThread threads
