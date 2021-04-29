module Main where

import Lib

main :: IO ()
main = do
  simpleTest
  runTest
  putStrLn "Generated images in '/tmp/out.png' and '/tmp/out-simple.png'"
