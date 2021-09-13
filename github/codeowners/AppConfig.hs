{-# LANGUAGE OverloadedStrings #-}
module AppConfig where

import           Configuration.Dotenv (Config (..), defaultConfig, loadFile)
import           Control.Monad        (when)
import qualified Data.ByteString.UTF8 as BSU
import           Data.Functor         (void, (<&>))
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import           GitHub.REST          (GitHubSettings (..), Token (AccessToken))
import           System.Directory     (doesFileExist, getCurrentDirectory,
                                       getHomeDirectory)
import           System.Environment   (getArgs, getEnv)
import           System.FilePath      (takeFileName)

data AppConfig = AppConfig { appConfigOwner          :: T.Text
                           , appConfigRepos          :: [T.Text]
                           , appConfigGitHubSettings :: GitHubSettings
                           }

defaultAppConfig :: IO AppConfig
defaultAppConfig = do
  loadConfigFile
  ghSettings <- githubSettingsFromEnv
  owner <- ownerFromEnv
  return AppConfig { appConfigOwner = owner
                   , appConfigRepos = []
                   , appConfigGitHubSettings = ghSettings
                   }

defaultGitHubSettings :: BSU.ByteString -> GitHubSettings
defaultGitHubSettings ghToken = GitHubSettings { token = Just $ AccessToken ghToken
                                               , userAgent = "sstoltze/haskell-app"
                                               , apiVersion = "v3"
                                               }

loadConfigFile :: IO ()
loadConfigFile = do
  userHome <- getHomeDirectory
  let envFile = userHome ++ "/.config/codeowners/.env"
  loadFiles [envFile, ".env"]
    where
      loadFiles [] = return ()
      loadFiles (f:fs) = do
        envExists <- doesFileExist f
        when envExists $ void $ loadFile $ defaultConfig { configPath = [f] }
        loadFiles fs

ownerFromEnv :: IO T.Text
ownerFromEnv = T.pack <$> getEnv "GITHUB_USER"

githubSettingsFromEnv :: IO GitHubSettings
githubSettingsFromEnv = defaultGitHubSettings . BSU.fromString <$> getEnv "GITHUB_PERSONAL_ACCESS_TOKEN"

appConfigFromArgs :: IO AppConfig
appConfigFromArgs = do
  args <- getArgs
  conf <- defaultAppConfig
  curDir <- T.pack . takeFileName <$> getCurrentDirectory
  return $ setDefaultDirectory curDir $ updateFromArgs conf args
    where
      updateFromArgs c []               = c
      updateFromArgs c ("--owner":o:as) = updateFromArgs (c { appConfigOwner = T.pack o }) as
      updateFromArgs c@AppConfig { appConfigRepos = repos } (f:as) = updateFromArgs (c { appConfigRepos = T.pack f : repos }) as
      setDefaultDirectory d c@AppConfig { appConfigRepos = [] } = c { appConfigRepos = [d] }
      setDefaultDirectory _ c = c
