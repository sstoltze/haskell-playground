{-# LANGUAGE OverloadedStrings #-}
module AppConfig where

import           Configuration.Dotenv (Config (..), defaultConfig, loadFile)
import           Control.Monad        (when)
import qualified Data.ByteString.UTF8 as BSU
import           Data.Functor         (void, (<&>))
import qualified Data.Text            as T
import           GitHub.REST          (GitHubSettings (..), Token (AccessToken))
import           System.Directory     (doesFileExist, getCurrentDirectory,
                                       getHomeDirectory)
import           System.Environment   (getArgs, getEnv)
import           System.FilePath      (takeFileName)

data AppConfig = AppConfig { appConfigOwner            :: T.Text
                           , appConfigRepos            :: [T.Text]
                           , appConfigGitHubSettings   :: GitHubSettings
                           , appConfigRemoveComments   :: Bool
                           , appConfigRemoveEmptyLines :: Bool
                           }

defaultAppConfig :: IO AppConfig
defaultAppConfig = do
  loadConfigFile
  ghSettings <- githubSettingsFromEnv
  owner <- ownerFromEnv
  return AppConfig { appConfigOwner = owner
                   , appConfigRepos = []
                   , appConfigGitHubSettings = ghSettings
                   , appConfigRemoveComments = False
                   , appConfigRemoveEmptyLines = False
                   }

defaultGitHubSettings :: String -> GitHubSettings
defaultGitHubSettings ghToken = GitHubSettings { token = Just $ AccessToken $ BSU.fromString ghToken
                                               , userAgent = "sstoltze/haskell-app"
                                               , apiVersion = "v3"
                                               }

loadConfigFile :: IO ()
loadConfigFile = do
  envFile <- getHomeDirectory <&> (++ "/.config/codeowners/.env")
  loadFiles [envFile, ".env"]
    where
      loadFiles [] = return ()
      loadFiles (f:fs) = do
        envExists <- doesFileExist f
        when envExists $
          void $ loadFile $ defaultConfig { configPath = [f] }
        loadFiles fs

ownerFromEnv :: IO T.Text
ownerFromEnv = T.pack <$> getEnv "GITHUB_USER"

githubSettingsFromEnv :: IO GitHubSettings
githubSettingsFromEnv = defaultGitHubSettings <$> getEnv "GITHUB_PERSONAL_ACCESS_TOKEN"

appConfigFromArgs :: IO AppConfig
appConfigFromArgs = do
  args <- getArgs
  conf <- defaultAppConfig
  curDir <- T.pack . takeFileName <$> getCurrentDirectory
  return $ setDefaultDirectory curDir $ updateFromArgs conf args
    where
      updateFromArgs c []                          = c
      updateFromArgs c ("--owner":o:as)            = updateFromArgs (c { appConfigOwner = T.pack o }) as
      updateFromArgs c ("--token":t:as)            = updateFromArgs (c { appConfigGitHubSettings = defaultGitHubSettings t }) as
      updateFromArgs c ("--remove-comments":as)    = updateFromArgs (c { appConfigRemoveComments = True }) as
      updateFromArgs c ("--remove-empty-lines":as) = updateFromArgs (c { appConfigRemoveEmptyLines = True }) as
      updateFromArgs c (f:as)                      = updateFromArgs (c { appConfigRepos = T.pack f : appConfigRepos c }) as
      setDefaultDirectory d c@AppConfig { appConfigRepos = [] } = c { appConfigRepos = [d] }
      setDefaultDirectory _ c = c
