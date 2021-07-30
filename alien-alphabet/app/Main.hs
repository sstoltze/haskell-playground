module Main where

import Digraph
import System.IO (isEOF, getLine)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- readInput
  let g = digraphFromAlphabeticalOrder input
  let cycle = fromMaybe [] $ digraphCycle g
  putStrLn (digraphDot $ digraphColourPath "red" cycle g)

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
