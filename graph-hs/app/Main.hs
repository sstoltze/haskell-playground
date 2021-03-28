module Main where

import Utility
import Svg (showSvgFile)

import System.Environment (getArgs)

main :: IO ()
main = do
  inputArgs <- getArgs
  let args@(Arguments { argWidth = w, argHeight = h }) = parseArgs inputArgs
  readChart args >>= putStrLn . showSvgFile w h
