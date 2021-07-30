module Utility ( Arguments(..)
               , parseArgs
               , readChart
               ) where

import Data.Functor ((<&>))

import Data.Char (isLetter)

import Types (Chart, DatasetType(..), Dataset, Colour, mkChart, mkDataset, mkColour)
import System.IO (IOMode(ReadMode), Handle, openFile, hIsEOF, hClose, hGetLine, stdin)

data InputType = Handle Handle
               | FilePath String

data Arguments = Arguments { argWidth :: Int
                           , argHeight :: Int
                           , argType :: DatasetType
                           , argHandle :: InputType
                           , argColours :: [Colour]
                           , argNames :: [String]
                           }

defaultArguments :: Arguments
defaultArguments = Arguments { argWidth = 400
                             , argHeight = 200
                             , argType = Line
                             , argHandle = Handle stdin
                             , argColours = defaultColours
                             , argNames = defaultNames
                             }

parseArgs :: [String] -> Arguments
parseArgs = parseArgs' defaultArguments
  where
    parseArgs' args [] = args
    parseArgs' args [opt]
      | opt == "--circle" || opt == "--circles" || opt == "-c" =
        args { argType = Circles }
      | otherwise =
        args
    parseArgs' args (opt:val:restArgs)
      | opt == "-w" || opt == "--width"  =
        parseArgs' (args { argWidth = read val }) restArgs
      | opt == "-h" || opt == "--height" =
        parseArgs' (args { argHeight = read val }) restArgs
      | opt == "--circle" || opt == "--circles" || opt == "-c" =
        parseArgs' (args { argType = Circles }) (val:restArgs)
      | opt == "-f" || opt == "--file" =
        parseArgs' (args { argHandle = FilePath val }) restArgs
      | otherwise =
        parseArgs' args restArgs

readPipe :: Arguments -> IO [String]
readPipe Arguments { argHandle = handle } = openArgHandle handle >>= readPipeAcc []
  where
    openArgHandle :: InputType -> IO Handle
    openArgHandle (Handle h) = return h
    openArgHandle (FilePath f) = openFile f ReadMode
    readPipeAcc :: [String] -> Handle -> IO [String]
    readPipeAcc acc h = do
      eof <- hIsEOF h
      if eof
        then hClose h >> return (reverse acc)
        else do
        inputLine <- hGetLine h
        readPipeAcc (inputLine : acc) h

toChart :: Arguments -> [String] -> Chart Double
toChart args@Arguments { argWidth = width, argHeight = height } inputLines =
  mkChart width height $ toDatasets updatedArgs $ linesToPoints input
  where
    (updatedArgs, input) = checkForTitles args inputLines
    checkForTitles a [] = (a, [])
    checkForTitles a (i:is) =
      if any isLetter i
      then (a { argNames = words i }, is)
      else (a, i:is)

readChart :: Arguments -> IO (Chart Double)
readChart args =
  readPipe args <&> toChart args

defaultColours :: [Colour]
defaultColours = [ mkColour 230 159 0
                 , mkColour 86 180 233
                 , mkColour 0 158 115
                 , mkColour 240 228 66
                 , mkColour 0 114 178
                 , mkColour 213 94 0
                 , mkColour 204 121 167
                 ]

defaultNames :: [String]
defaultNames = [ "Plot 1"
               , "Plot 2"
               , "Plot 3"
               , "Plot 4"
               , "Plot 5"
               , "Plot 6"
               , "Plot 7"
               ]

toDatasets :: Arguments -> [[Double]] -> [Dataset Double]
toDatasets Arguments { argWidth = end, argType = dataType, argColours = colours, argNames = names }
  = zipWith3 (mkDataset 0 (fromIntegral end) dataType) colours names

linesToPoints :: [String] -> [[Double]]
linesToPoints [] = repeat []
linesToPoints (l:ls) = zipWith (:) currentYs restYs
  where
    currentYs :: [Double]
    currentYs = map read $ words l
    restYs :: [[Double]]
    restYs = linesToPoints ls
