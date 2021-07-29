module Main where

import Lib
import System.IO

main :: IO ()
main = do
  input <- readInput
  putStrLn (digraphDot $ digraphFromAlphabeticalOrder input)

readInput :: IO [String]
readInput = readInput' []
  where
    readInput' acc = do
      eof <- isEOF
      if eof
        then return $ reverse acc
        else do
        inputLine <- getLine
        readInput' (inputLine : acc)
