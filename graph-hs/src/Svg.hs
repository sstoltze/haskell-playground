module Svg where

import Data.List (intercalate, intersperse)
import Types
import Text.Printf

labelWidth :: Int
labelWidth = 300

labelMargin :: Int
labelMargin = 30

svgHeader :: Int -> Int -> String
svgHeader w h =
  "<?xml version='1.0'?>\n<svg xmlns='http://www.w3.org/2000/svg' width='"
  ++ show (w + labelWidth)
  ++ "' height='"
  ++ show h
  ++ "' version='1.1'>\n"

svgFooter :: String
svgFooter = "\n</svg>"

toSvg :: (Svg a) => Int -> Int -> a -> String
toSvg width height s =
  svgHeader width height
  ++ showSvg s
  ++ svgFooter

class Svg a where
  showSvg :: a -> String
  showSvgFile :: Int -> Int -> a -> String
  showSvgFile w h a = toSvg w h a

groupSvg :: Svg a => [a] -> String
groupSvg as =
  "<g>\n" ++ concat svgElements ++ "</g>"
  where
    svgElements = intersperse "\n" $ map showSvg as

instance Svg a => Svg [a] where
  showSvg = groupSvg

svgPoint :: (PrintfArg a) => Point a -> String
svgPoint p = printf "%.2f" (pointX p) ++ "," ++ printf "%.2f" (pointY p)

svgTitles :: (PrintfArg a) => Chart a -> String
svgTitles c = svgTitles' "" 1 (chartData c)
  where
    svgTitles' acc _ [] = acc
    svgTitles' acc i (d:ds) = svgTitles' (acc ++ svgTitle i d) (i+1) ds
    svgTitle i d =
      let colour = hexColour $ datasetColour d in
      printf "<g transform='translate(%d %d)'>\n" (chartWidth c + labelMargin) (i * 20 :: Int)
      ++ printf "<circle cx='-10' cy='-7' r='3.5' fill='%s' stroke='%s'/>\n" colour colour
      ++ printf "<text style='fill: %s; font-size: 15px; font-family: mono'>%s [%.2f,%.2f]</text>" colour (datasetName d) (datasetMinimum d) (datasetMaximum d)
      ++ "</g>"

svgLine :: (PrintfArg a) => Dataset a -> String
svgLine d =
  "<polyline stroke='"
  ++ colour
  ++ "' stroke-width='1.5' fill='none' points='"
  ++ svgPoints
  ++ "'/>\n"
  where
    colour = hexColour $ datasetColour d
    svgPoints = foldr (\p r -> svgPoint p ++ " " ++ r) "" $ datasetPoints d

svgCircle :: (PrintfArg a) => Dataset a -> String
svgCircle d = concat svgCircles
  where
    colour = hexColour $ datasetColour d
    pointToSvgCircle p =
      printf "<circle cx='%.2f' cy='%.2f' r='1.2' fill='%s' stroke='%s' />\n" (pointX p) (pointY p) colour colour
    svgCircles = pointToSvgCircle <$> datasetPoints d

instance PrintfArg a => Svg (Dataset a) where
  showSvg d@Dataset { datasetType = Line } = svgLine d
  showSvg d@Dataset { datasetType = Circles } = svgCircle d

svgChart :: (PrintfArg a) => Chart a -> String
svgChart c =
  svgDatasets
  ++ "\n"
  ++ svgTitles c
  where
    datasets = chartData c
    svgDatasets = intercalate "\n" $ map showSvg datasets

svgScaleDataset :: (Fractional a, Ord a) => a -> a -> Dataset a -> Dataset a
svgScaleDataset width height d@Dataset {datasetPoints = points} =
  d { datasetPoints = fmap scale points }
  where
    ys = map pointY points
    minY = minimum ys
    maxY = maximum ys
    deltaY = maxY - minY
    xs = map pointX points
    minX = minimum xs
    maxX = maximum xs
    deltaX = maxX - minX
    scale p = (width * (pointX p - minX) / deltaX, height - height * (pointY p - minY) / deltaY)

svgScaleChart :: (Fractional a, Ord a) => Chart a -> Chart a
svgScaleChart c@Chart { chartWidth = w, chartHeight = h, chartData = d } =
  c { chartData = fmap (svgScaleDataset (fromIntegral w) (fromIntegral h)) d }

instance (PrintfArg a, Fractional a, Ord a) => Svg (Chart a) where
  showSvg = svgChart . svgScaleChart
