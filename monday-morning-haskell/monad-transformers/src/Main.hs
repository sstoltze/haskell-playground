{-# LANGUAGE OverloadedStrings, ApplicativeDo, LambdaCase #-}

module Main where

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data LoginError =
  InvalidEmail | NoSuchUser | WrongPassword
  deriving Show

users :: Map Text Text
users = Map.fromList [ ("example.com", "qwerty123")
                     , ("localhost", "password")]

main :: IO ()
main = return ()

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [_, domain] -> Right domain
    _ -> Left InvalidEmail

printResult :: Either LoginError Text -> IO ()
printResult =
  T.putStrLn . either
    (const "ERROR: Invalid domain")
    (append "Domain: ")

validateToken :: Either LoginError Text -> Either LoginError Text
validateToken (Right domain) =
  case Map.lookup domain users of
    Just pw -> Right pw
    Nothing -> Left NoSuchUser
validateToken x = x

getToken'' :: IO (Either LoginError Text)
getToken'' = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)

userLogin' :: IO (Either LoginError Text)
userLogin' = do
  token <- getToken''
  -- This is hard to rewrite in a nice way since we mix IO and Either monads
  case validateToken token of
    Right pw -> do
      T.putStrLn "Enter password:"
      userPW <- T.getLine
      if userPW == pw
        then return token
        else return (Left WrongPassword)
    x -> return x

-- Combine IO and Either
data ExceptIO e a = ExceptIO {
  runExceptIO :: IO (Either e a)
}

-- This defines ways of moving back and forth:
-- :t ExceptIO
-- ExceptIO :: IO (Either e a) -> ExceptIO e a
-- :t runExceptIO
-- runExceptIO :: ExceptIO e a -> IO (Either e a)

instance Functor (ExceptIO e) where
  -- To map, first unwrap, then map, then wrap again
  fmap f = ExceptIO . fmap (fmap f) . runExceptIO

instance Applicative (ExceptIO e) where
  pure = ExceptIO . return . Right
  f <*> x = ExceptIO { runExceptIO = runExceptIO' } where
    runExceptIO' = do
      f' <- runExceptIO f
      x' <- runExceptIO x
      return (f' <*> x')

instance Monad (ExceptIO e) where
  x >>= f = ExceptIO { runExceptIO = runExceptIO' } where
    runExceptIO' = do
      x' <- runExceptIO x
      case x' of
        Right x'' -> runExceptIO $ f x''
        Left e -> return $ Left e

-- This looks pretty horrible...
getToken' :: ExceptIO LoginError Text
getToken' = do
  ExceptIO (fmap Right (T.putStrLn "Enter email address:"))
  input <- ExceptIO (fmap Right T.getLine)
  ExceptIO $ return (getDomain input)

liftEither :: Either e a -> ExceptIO e a
liftEither = ExceptIO . return

liftIO :: IO a -> ExceptIO e a
liftIO = ExceptIO . fmap Right

getToken :: ExceptIO LoginError Text
getToken = do
  liftIO $ T.putStrLn "Enter email address:"
  input <- liftIO T.getLine
  liftEither $ getDomain input

userLogin :: ExceptIO LoginError Text
userLogin = do
  token <- getToken
  userpw <- maybe (throwE NoSuchUser) return (Map.lookup token users)
  pw <- liftIO (T.putStrLn "Enter your password:" >> T.getLine)
  if userpw == pw
    then return token
    else throwE WrongPassword

throwE :: e -> ExceptIO e a
throwE x = liftEither (Left x)

-- Rename EitherIO to ExceptIO throughout code, since we are basically working with exceptions

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler =
  ExceptIO $ do
    x <- runExceptIO throwing
    case x of
      Left err -> runExceptIO $ handler err
      success -> return success

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO $ T.putStrLn "Wrong password, one more chance."
  userLogin
wrongPasswordHandler e = throwE e

printError :: LoginError -> ExceptIO LoginError a
printError e = do
  liftIO . T.putStrLn $ case e of
    WrongPassword -> "Wrong password. No more chances."
    NoSuchUser -> "No user with that email address."
    InvalidEmail -> "Invalid email address entered."
  throwE e

loginDialogue :: ExceptIO LoginError ()
loginDialogue = do
  let retry = catchE userLogin wrongPasswordHandler
  token <- catchE retry printError
  liftIO . T.putStrLn $ append "Logged in with token: " token

-- ExceptIO can be translated to ExceptT by replacing IO with a monad
data ExceptT e m a = ExceptT {
  runExceptT :: m (Either e a)
}

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  -- Combine m (Either) and m (Either) to a single m (Either) and then fmap <*> into the Eithers
  f <*> x = ExceptT $ (<*>) <$> runExceptT f <*> runExceptT x

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  x >>= f = ExceptT $ (runExceptT x) >>= \case
    Left e -> return (Left e)
    Right a -> runExceptT (f a)

lift :: (Functor m) => m a -> ExceptT e m a
lift = ExceptT . fmap Right
