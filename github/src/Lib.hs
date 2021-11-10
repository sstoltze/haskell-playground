{-# LANGUAGE OverloadedStrings #-}
module Lib (repoCodeowners) where

import qualified Data.Aeson                as Aeson
import           Data.Either               (partitionEithers)
import qualified Data.Text                 as T
import           Data.Text.Encoding.Base64 (decodeBase64)
import           GitHub.REST               (EndpointVals, GHEndpoint (..),
                                            GitHubT, KeyValue (..), githubTry',
                                            queryGitHub, queryGitHubAll, (.:))
import           Network.HTTP.Types        (StdMethod (GET), status404)

ghGetRequest :: T.Text -> EndpointVals -> GHEndpoint
ghGetRequest ghEndpoint values =
  GHEndpoint { method = GET
             , endpoint = ghEndpoint
             , endpointVals = values
             , ghData = []
             }

orgReposRequest :: T.Text -> GHEndpoint
orgReposRequest org = ghGetRequest "/orgs/:org/repos" [ "org" := org ]

getContentRequest :: T.Text -> T.Text -> T.Text -> GHEndpoint
getContentRequest owner repo path =
  ghGetRequest "/repos/:owner/:repo/contents/:path" [ "owner" := owner
                                                    , "repo" := repo
                                                    , "path" := path
                                                    ]

orgReposAll :: T.Text -> GitHubT IO [Aeson.Value]
orgReposAll = queryGitHubAll . orgReposRequest

repoContent :: T.Text -> T.Text -> T.Text -> GitHubT IO Aeson.Value
repoContent owner repo path = queryGitHub $ getContentRequest owner repo path

repoCodeowners :: T.Text -> T.Text -> GitHubT IO (Either T.Text T.Text)
repoCodeowners owner repo = do
  tryGetOwners <- githubTry' status404 (repoContent owner repo "CODEOWNERS")
  case tryGetOwners of
    Left _          -> return $ Left "No codeowners file in repository."
    Right jsonValue -> return $ decodeCodeowners $ jsonValue .: "content"

reposCodeowners :: T.Text -> [T.Text] -> GitHubT IO [Either T.Text T.Text]
reposCodeowners owner = mapM (repoCodeowners owner)

repoName :: Aeson.Value -> T.Text
repoName r = r .: "name"

decodeCodeowners :: T.Text -> Either T.Text T.Text
decodeCodeowners s = if null failed
  then Right $ T.concat succeeded
  else Left $ T.append "Could not base64 decode: " $ head failed
  where
    encodedOwnerLines = T.splitOn "\n" $ T.strip s
    (failed, succeeded) = partitionEithers $ fmap decodeBase64 encodedOwnerLines
