module Graph where

import Types
import System.IO

readPipe :: IO [String]
readPipe = readPipeAcc []
  where
    readPipeAcc :: [String] -> IO [String]
    readPipeAcc acc = do
      eof <- isEOF
      if eof
        then return $ reverse acc
        else do
        inputLine <- getLine
        readPipeAcc (inputLine : acc)

colours :: [Colour]
colours = [ mkColour 230 159 0
          , mkColour 86 180 233
          , mkColour 0 158 115
          , mkColour 240 228 66
          , mkColour 0 114 178
          , mkColour 213 94 0
          , mkColour 204 121 167
          ]

toDatasets :: Double -> Double -> [[Double]] -> [Dataset Double]
toDatasets start end = zipWith (mkDataset start end) colours

linesToPoints :: [String] -> [[Double]]
linesToPoints [] = repeat []
linesToPoints (l:ls) = addYs currentYs restYs
  where
    currentYs :: [Double]
    currentYs = map read $ words l
    restYs :: [[Double]]
    restYs = linesToPoints ls
    addYs :: [Double] -> [[Double]] -> [[Double]]
    addYs = zipWith (:)

toChart :: Int -> Int -> [String] -> Chart Double
toChart width height = mkChart width height . toDatasets 0 (fromIntegral width) . linesToPoints

readChart :: Int -> Int -> IO (Chart Double)
readChart width height = readPipe >>= return . toChart width height
