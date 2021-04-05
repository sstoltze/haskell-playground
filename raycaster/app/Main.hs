module Main where

import Lib

main :: IO ()
main = do
  runTest
  putStrLn "Generated image in '/tmp/out.png'"
