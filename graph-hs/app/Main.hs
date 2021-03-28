module Main where

import Graph
import Svg (showSvgFile)
import System.Environment

main :: IO ()
main = do
  inputArgs <- getArgs
  let args@(Arguments { argWidth = w, argHeight = h }) = parseArgs inputArgs
  readChart args >>= putStrLn . showSvgFile w h
