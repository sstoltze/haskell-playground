module Main where

import Lib

main :: IO ()
main = do
  simpleTest
  --runTest
  putStrLn "Generated image in '/tmp/out-simple.png'"
