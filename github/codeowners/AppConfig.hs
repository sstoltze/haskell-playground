{-# LANGUAGE OverloadedStrings #-}

module AppConfig (AppConfig (..), AppConfigRepo (..), appConfigFromArgs) where

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

data AppConfigRepo = AppConfigRepo
  { repoRepo  :: T.Text,
    repoOwner :: T.Text
  }

data AppConfig = AppConfig
  { appConfigRepos            :: [AppConfigRepo],
    appConfigGitHubSettings   :: GitHubSettings,
    appConfigRemoveComments   :: Bool,
    appConfigRemoveEmptyLines :: Bool
  }

defaultAppConfig :: IO AppConfig
defaultAppConfig = do
  ghSettings <- githubSettingsFromEnv
  return
    AppConfig
      { appConfigRepos = [],
        appConfigGitHubSettings = ghSettings,
        appConfigRemoveComments = False,
        appConfigRemoveEmptyLines = False
      }

defaultGitHubSettings :: String -> GitHubSettings
defaultGitHubSettings ghToken =
  GitHubSettings
    { token = Just $ AccessToken $ BSU.fromString ghToken,
      userAgent = "sstoltze/haskell-app",
      apiVersion = "v3"
    }

loadConfigFile :: IO ()
loadConfigFile = do
  envFile <- getHomeDirectory <&> (++ "/.config/codeowners/.env")
  loadFiles [envFile, ".env"]
  where
    loadFiles [] = return ()
    loadFiles (f : fs) = do
      envExists <- doesFileExist f
      when envExists $
        void $ loadFile $ defaultConfig {configPath = [f]}
      loadFiles fs

ownerFromEnv :: IO T.Text
ownerFromEnv = T.pack <$> getEnv "GITHUB_USER"

githubSettingsFromEnv :: IO GitHubSettings
githubSettingsFromEnv = defaultGitHubSettings <$> getEnv "GITHUB_PERSONAL_ACCESS_TOKEN"

appConfigFromArgs :: IO AppConfig
appConfigFromArgs = do
  args <- getArgs
  loadConfigFile
  conf <- defaultAppConfig
  defaultOwner <- ownerFromEnv
  curDir <- T.pack . takeFileName <$> getCurrentDirectory
  return $ setDefaultDirectory defaultOwner curDir $ updateFromArgs conf defaultOwner args
  where
    updateFromArgs c _ [] = c {appConfigRepos = reverse $ appConfigRepos c}
    updateFromArgs c _ ("--owner" : o : as) = updateFromArgs c (T.pack o) as
    updateFromArgs c currentOwner ("--token" : t : as) = updateFromArgs (c {appConfigGitHubSettings = defaultGitHubSettings t}) currentOwner as
    updateFromArgs c currentOwner ("--remove-comments" : as) = updateFromArgs (c {appConfigRemoveComments = True}) currentOwner as
    updateFromArgs c currentOwner ("--remove-empty-lines" : as) = updateFromArgs (c {appConfigRemoveEmptyLines = True}) currentOwner as
    updateFromArgs c currentOwner (f : as) = updateFromArgs (c {appConfigRepos = AppConfigRepo {repoRepo = T.pack f, repoOwner = currentOwner} : appConfigRepos c}) currentOwner as
    setDefaultDirectory o d c@AppConfig {appConfigRepos = []} = c {appConfigRepos = [AppConfigRepo {repoRepo = d, repoOwner = o}]}
    setDefaultDirectory _ _ c = c
