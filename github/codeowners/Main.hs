{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad
import           Data.Either       (partitionEithers)
import qualified Data.Text         as T
import           GitHub.REST
import qualified Streaming.Prelude as S

import           AppConfig
import           Lib

-- TODO: Go to git root before getting default repo name
main :: IO ()
main = do
  conf <- appConfigFromArgs
  let repos = appConfigRepos conf
  let s = S.each repos :: S.Stream (S.Of AppConfigRepo) IO ()
  S.mapM_ (printCodeowner conf) $ S.zip (S.map repoRepo s) $ S.mapM (getRepoOwner conf) s
    where
      getRepoOwner conf repo = runGitHubT (appConfigGitHubSettings conf) $ repoCodeowners (repoOwner repo) (repoRepo repo)

printCodeowner :: AppConfig -> (T.Text, Either T.Text T.Text) -> IO ()
printCodeowner _ (r, Left e)  = putStr $ T.unpack $ T.concat ["Error getting owners for ", r, ":\n", e, "\n"]
printCodeowner c (r, Right s) = putStr $ T.unpack $ T.concat ["Owners for ", r, ":\n", formatCodeowner c s, "\n"]

formatCodeowner :: AppConfig -> T.Text -> T.Text
formatCodeowner c = removeEmptyLines c . removeComments c

removeComments :: AppConfig -> T.Text -> T.Text
removeComments AppConfig { appConfigRemoveComments = False } = id
removeComments AppConfig { appConfigRemoveComments = True } =
  T.unlines . filter (not . T.isPrefixOf "#" . T.strip) . T.lines

removeEmptyLines :: AppConfig -> T.Text -> T.Text
removeEmptyLines AppConfig { appConfigRemoveEmptyLines = False } = id
removeEmptyLines AppConfig { appConfigRemoveEmptyLines = True } =
  T.unlines . filter (not . T.null) . T.lines
