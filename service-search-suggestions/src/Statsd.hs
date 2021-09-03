{-# LANGUAGE OverloadedStrings #-}

module Statsd (HandlerCounter(..),
               statsdClient,
               setupStatsdStore,
               statsdStoreFromEnv,
               createHandlerCounter,
               statsdHandlerRequest,
               statsdHandlerSuccess,
               statsdHandlerFailure) where

import           Configuration.Dotenv            (defaultConfig, loadFile)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text                       as T
import           System.Environment              (lookupEnv)
import           System.Metrics
import qualified System.Metrics.Counter          as Counter
import           System.Remote.Monitoring.Statsd

import           Types.Handler
import           Types.Statsd

statsdClient :: T.Text -> T.Text -> StatsdOptions
statsdClient sdHost sdPrefix = defaultStatsdOptions { host = sdHost
                                                    , prefix = sdPrefix
                                                    }

setupStatsdStore :: StatsdOptions -> IO Store
setupStatsdStore opts = do
  store <- newStore
  _ <- forkStatsd opts store
  return store

statsdStoreFromEnv :: IO Store
statsdStoreFromEnv = do
  void $ loadFile defaultConfig
  sdHost <- maybe "" T.pack <$> lookupEnv "STATSD_HOST"
  sdPrefix <- maybe "" T.pack <$> lookupEnv "STATSD_PREFIX"
  setupStatsdStore $ statsdClient sdHost sdPrefix

createHandlerCounter :: Store -> T.Text -> IO HandlerCounter
createHandlerCounter store routingKey = do
  reqCounter <- createCounter (routingKey <> ".count") store
  sucCounter <- createCounter (routingKey <> ".success") store
  errCounter <- createCounter (routingKey <> ".failure") store
  return $ HandlerCounter { requestCounter = reqCounter
                          , successCounter = sucCounter
                          , failureCounter = errCounter
                          }

counterRequest :: (MonadIO m) => HandlerCounter -> m ()
counterRequest = liftIO . Counter.inc . requestCounter

counterSuccess :: (MonadIO m) => HandlerCounter -> m ()
counterSuccess = liftIO . Counter.inc . successCounter

counterFailure :: (MonadIO m) => HandlerCounter -> m ()
counterFailure = liftIO . Counter.inc . failureCounter

statsdHandlerRequest :: HandlerIO ()
statsdHandlerRequest = do
  context <- ask
  counterRequest $ contextCounter context

statsdHandlerSuccess :: HandlerIO ()
statsdHandlerSuccess = do
  context <- ask
  counterSuccess $ contextCounter context

statsdHandlerFailure :: HandlerIO ()
statsdHandlerFailure = do
  context <- ask
  counterFailure $ contextCounter context
