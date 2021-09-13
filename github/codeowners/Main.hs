{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Either   (partitionEithers)
import qualified Data.Text     as T
import           GitHub.REST

import           AppConfig
import           Lib

-- TODO: Go to git root before getting default repo name
main :: IO ()
main = do
  conf <- appConfigFromArgs
  forM_ (appConfigRepos conf) $ \repo -> do
    owner <- runGitHubT (appConfigGitHubSettings conf) $ do
      codeowner <- repoCodeowners (appConfigOwner conf) repo
      return (repo, codeowner)
    printCodeowner owner

printCodeowner :: (T.Text, Either T.Text T.Text) -> IO ()
printCodeowner (r, Left e)  = putStrLn $ T.unpack $ T.concat ["Error getting owners for ", r, ":\n", e, "\n"]
printCodeowner (r, Right s) = putStr $ T.unpack $ T.concat ["Owners for ", r, ":\n", s, "\n"]
