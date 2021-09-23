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
  forM_ (appConfigRepos conf) $ \(AppConfigRepo o repo) -> do
    owner <- runGitHubT (appConfigGitHubSettings conf) $ do
      codeowner <- repoCodeowners o repo
      return (repo, codeowner)
    printCodeowner conf owner

printCodeowner :: AppConfig -> (T.Text, Either T.Text T.Text) -> IO ()
printCodeowner _ (r, Left e)  = putStr $ T.unpack $ T.concat ["Error getting owners for ", r, ":\n", e, "\n"]
printCodeowner c (r, Right s) = putStr $ T.unpack $ T.concat ["Owners for ", r, ":\n", formatCodeowner c s, "\n"]

formatCodeowner :: AppConfig -> T.Text -> T.Text
formatCodeowner c = removeEmptyLines c . removeComments c

removeComments :: AppConfig -> T.Text -> T.Text
removeComments (AppConfig { appConfigRemoveComments = False }) = id
removeComments (AppConfig { appConfigRemoveComments = True }) =
  T.unlines . filter (not . T.isPrefixOf "#" . T.strip) . T.lines

removeEmptyLines :: AppConfig -> T.Text -> T.Text
removeEmptyLines (AppConfig { appConfigRemoveEmptyLines = False }) = id
removeEmptyLines (AppConfig { appConfigRemoveEmptyLines = True }) =
  T.unlines . filter (not . T.null) . T.lines
