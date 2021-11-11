module Generate where

import           Data.ProtoLens    (defMessage, showMessage)
import qualified Data.Text         as T
import           Lens.Micro
import           Test.QuickCheck

import qualified Proto.Test        as Test
import qualified Proto.Test_Fields as Test

randomChar :: Gen Char
randomChar = choose ('a', 'z')

randomString :: Gen String
randomString = listOf randomChar

randomStringWithLength :: (Int, Int) -> Gen String
randomStringWithLength bounds = choose bounds >>= flip vectorOf randomChar

buildTestRequest :: IO Test.TestRequest
buildTestRequest = do
  query <- generate $ randomStringWithLength (4, 15)
  return $ defMessage & Test.query .~ (T.pack query)
