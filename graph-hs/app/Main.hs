module Main where

import Graph
import Svg

chartWidth :: Int
chartWidth = 400

chartHeight :: Int
chartHeight = 200

chartGutter :: Int
chartGutter = 10

main :: IO ()
main = readChart chartWidth chartHeight >>= putStrLn . showSvgFile chartWidth chartHeight
