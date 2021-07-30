module Main where

import Lib
import System.IO

main :: IO ()
main = do
  input <- readInput
  let g = digraphFromAlphabeticalOrder input
  let cycle = case digraphCycle g of
        Just c -> c
        Nothing -> []
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
