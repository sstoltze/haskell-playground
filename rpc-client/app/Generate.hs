{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate where

import           Data.ProtoLens    (defMessage)
import qualified Data.Text         as T
import           Data.Word         (Word64)
import           Lens.Micro
import qualified Proto.Test        as Test
import qualified Proto.Test_Fields as Test
import           Test.QuickCheck

class HasGenerate a where
  gen :: IO a

randomChar :: Gen Char
randomChar = choose ('a', 'z')

randomString :: Gen String
randomString = listOf randomChar

randomStringWithLength :: (Int, Int) -> Gen String
randomStringWithLength bounds = choose bounds >>= flip vectorOf randomChar

chooseBetween :: Gen a -> [a] -> Gen a
chooseBetween ga l = do
  prob <- chooseInt (1, 2)
  if prob < 2 then ga else elements l

instance (HasGenerate String) where
  gen = generate $ chooseBetween (randomStringWithLength (4, 15)) ["test"]

runGen :: (HasGenerate a) => (a -> b) -> IO b
runGen f = f <$> gen

instance (HasGenerate T.Text) where
  gen = runGen T.pack

buildTestRequest :: String -> Test.TestRequest
buildTestRequest query = defMessage & Test.query .~ T.pack query

instance (HasGenerate Test.TestRequest) where
  gen = runGen buildTestRequest

generateTestRequest :: IO Test.TestRequest
generateTestRequest = gen

genRandomUserId :: Gen Data.Word.Word64
genRandomUserId = choose (1000000, 5000000)

genUserId :: Gen Data.Word.Word64
genUserId = genRandomUserId

genRandomUsername :: Gen T.Text
genRandomUsername = T.pack <$> randomStringWithLength (5, 15)

genUsername :: Gen T.Text
genUsername = genRandomUsername

genRandomEmail :: Gen T.Text
genRandomEmail = do
  name <- genUsername
  domain <- T.pack <$> elements ["com", "dk", "de", "pt"]
  domainName <- genUsername
  return $ T.concat [name, "@", domainName, ".", domain]

genEmail :: Gen T.Text
genEmail = genRandomEmail
