module Results where

import           Control.Concurrent

data Results = Results
  { resultsSuccess :: Int,
    resultsFailure :: Int
  }
  deriving (Show)

incSuccess :: Results -> Results
incSuccess r = r {resultsSuccess = 1 + resultsSuccess r}

incFailure :: Results -> Results
incFailure r = r {resultsFailure = 1 + resultsFailure r}

newResults :: IO (MVar Results)
newResults = newMVar $ Results 0 0

putResult :: MVar Results -> Either a b -> IO ()
putResult results resp =
  modifyMVar_ results $
    return
      . case resp of
        Left _  -> incFailure
        Right _ -> incSuccess

printResults :: MVar Results -> IO ()
printResults r = readMVar r >>= print
