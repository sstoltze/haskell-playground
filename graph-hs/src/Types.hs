module Types where

import Data.Char ( intToDigit )

data Colour = Colour { colourRed :: Int
                     , colourGreen :: Int
                     , colourBlue :: Int
                     } deriving Show

mkColour :: Int -> Int -> Int -> Colour
mkColour r g b = Colour { colourRed = r, colourGreen = g, colourBlue = b }

hexColour :: Colour -> String
hexColour (Colour { colourRed = r, colourGreen = g, colourBlue = b}) =
  "#" ++ hex r ++ hex g ++ hex b
  where
    hex n = intToDigit (n `div` 16) : intToDigit (n `mod` 16) : []

type Point a = (a, a)

mkPoint :: a -> a -> Point a
mkPoint x y = (x,y)

pointX :: Point a -> a
pointX = fst

pointY :: Point a -> a
pointY = snd

scalePoint :: (Num a) => a -> Point a -> Point a
scalePoint d (x,y) = (x, d * y)

mkRange :: (Enum a, Fractional a) => a -> a -> a -> [a]
mkRange start end numberOfPoints = [start, start+step .. end]
  where
    step = (end - start) / (numberOfPoints - 1)

uniformPoints :: (Enum a, Fractional a) => a -> a -> [a] -> [Point a]
uniformPoints start end ys = zipWith mkPoint xs ys
  where
    len = length ys
    xs = mkRange start end (fromIntegral len)

data DatasetType = Line
                 | Circles
                 deriving (Show, Eq)

data Dataset a = Dataset { datasetPoints :: [Point a]
                         , datasetColour :: Colour
                         , datasetType :: DatasetType
                         , datasetName :: String
                         , datasetMinimum :: a
                         , datasetMaximum :: a
                         } deriving Show

mkDataset :: (Ord a, Enum a, Fractional a) => a -> a -> DatasetType -> Colour -> String -> [a] -> Dataset a
mkDataset start end dataType colour name points = Dataset { datasetPoints = uniformPoints start end points
                                                          , datasetColour = colour
                                                          , datasetType = dataType
                                                          , datasetName = name
                                                          , datasetMinimum = minimum points
                                                          , datasetMaximum = maximum points
                                                          }

data Chart a = Chart { chartData :: [Dataset a]
                     , chartWidth :: Int
                     , chartHeight :: Int
                     } deriving Show

mkChart :: Int -> Int -> [Dataset a] -> Chart a
mkChart width height datasets = Chart { chartData = datasets
                                      , chartWidth = width
                                      , chartHeight = height
                                      }
