{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Configuration.Dotenv (defaultConfig, loadFile)
import           Data.Functor         (void)
import           Data.Maybe           (fromJust)
import           Data.ProtoLens       (defMessage, showMessage)
import qualified Data.Text            as T
import           GHC.Word             (Word32)
import           Lens.Micro
import           System.Environment   (lookupEnv)

import qualified Proto.Test           as Test
import qualified Proto.Test_Fields    as Test

import           Amqp
import           Client

buildTestRequest :: T.Text -> Test.TestRequest
buildTestRequest query =
  defMessage
  & Test.query .~ query

main :: IO ()
main = do
  void (loadFile defaultConfig)
  conn <- amqpConnectionFromEnv
  channel <- createChannel conn
  client <- makeRpcClient channel
  let searchQuery = "test"
  let request = buildTestRequest searchQuery
  routingKey <- T.pack . fromJust <$> lookupEnv "ROUTING_KEY"
  reply <- call client routingKey request :: IO (Either String Test.TestResponse)
  putStrLn $ either id showMessage reply
