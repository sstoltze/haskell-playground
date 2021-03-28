module Graph where

import Types

import System.IO

colours :: [Colour]
colours = [ mkColour 230 159 0
          , mkColour 86 180 233
          , mkColour 0 158 115
          , mkColour 240 228 66
          , mkColour 0 114 178
          , mkColour 213 94 0
          , mkColour 204 121 167
          ]

toDatasets :: Double -> Double -> DatasetType -> [[Double]] -> [Dataset Double]
toDatasets start end dataType = zipWith (mkDataset start end dataType) colours

linesToPoints :: [String] -> [[Double]]
linesToPoints [] = repeat []
linesToPoints (l:ls) = zipWith (:) currentYs restYs
  where
    currentYs :: [Double]
    currentYs = map read $ words l
    restYs :: [[Double]]
    restYs = linesToPoints ls

toChart :: Arguments -> [String] -> Chart Double
toChart (Arguments { argWidth = width, argHeight = height, argType = dataType}) =
  mkChart width height . toDatasets 0 (fromIntegral width) dataType . linesToPoints

defaultChartWidth :: Int
defaultChartWidth = 400

defaultChartHeight :: Int
defaultChartHeight = 200

data InputType = Handle Handle
               | FilePath String

data Arguments = Arguments { argWidth :: Int
                           , argHeight :: Int
                           , argType :: DatasetType
                           , argHandle :: InputType
                           }

defaultArguments :: Arguments
defaultArguments = Arguments { argWidth = defaultChartWidth
                             , argHeight = defaultChartHeight
                             , argType = Line
                             , argHandle = Handle stdin
                             }

readPipe :: Arguments -> IO [String]
readPipe (Arguments { argHandle = handle }) = openArgHandle handle >>= readPipeAcc []
  where
    openArgHandle :: InputType -> IO Handle
    openArgHandle (Handle h) = return h
    openArgHandle (FilePath f) = openFile f ReadMode
    readPipeAcc :: [String] -> Handle -> IO [String]
    readPipeAcc acc h = do
      eof <- hIsEOF h
      if eof
        then hClose h >> (return $ reverse acc)
        else do
        inputLine <- hGetLine h
        readPipeAcc (inputLine : acc) h

readChart :: Arguments -> IO (Chart Double)
readChart args =
  readPipe args >>= return . toChart args

parseArgs :: [String] -> Arguments
parseArgs cliArgs = parseArgs' defaultArguments cliArgs
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
