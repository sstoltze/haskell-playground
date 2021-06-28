{-# LANGUAGE OverloadedStrings #-}

-- Following https://github.com/cdupont/keycloak-hs/blob/master/docs/tutorial.md

module Main where

import Keycloak
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = do
  -- Had to set ream to confidential (which required adding a redirect) and remove the / from the auth-server-url
  kcConfig <- configureKeycloak "keycloak.json"
  void $ flip runKeycloak kcConfig $ do
    jwt <- getJWT "demo" "demo"
    claims <- verifyJWT jwt
    liftIO $ putStrLn $ "Claims decoded from token:\n" ++ (show claims) ++ "\n\n"
    let user = getClaimsUser claims
    liftIO $ putStrLn $ "User decoded from claims:\n" ++ (show user) ++ "\n\n"
    let resourceId = ResourceId "SecretStuff"
    isAuth <- isAuthorized resourceId (ScopeName "view") jwt
    liftIO $ putStrLn $ "User has access to resource:\n" ++ (show isAuth) ++ "\n\n"
    -- This required going to Users -> demo -> Role Mappings -> Client Roles -> demo -> Add uma_protection, and it still does not work
    resource <- getResource resourceId jwt
    liftIO $ putStrLn $ "Resource:\n" ++ (show resource)
