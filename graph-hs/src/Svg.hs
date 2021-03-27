module Svg where

import Types
import Text.Printf

svgHeader :: Int -> Int -> String
svgHeader w h =
  "<?xml version='1.0'?>\n<svg xmlns='http://www.w3.org/2000/svg' width='"
  ++ show w
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

svgPoint :: (PrintfArg a) => Point a -> String
svgPoint (a,b) = printf "%.2f" a ++ "," ++ printf "%.2f" b

svgLine :: (PrintfArg a) => Dataset a -> String
svgLine d = "<polyline stroke='" ++ colour ++ "' stroke-width='1.5' fill='none' points='" ++ svgPoints ++ "'/>"
  where
    colour = hexColour $ datasetColour d
    svgPoints = foldr (\p r -> svgPoint p ++ " " ++ r) "" $ datasetPoints d

instance PrintfArg a => Svg (Dataset a) where
  showSvg = svgLine

svgChart :: (PrintfArg a) => Chart a -> String
svgChart c = svgLines
  where
    datasets = chartData c
    svgLines = foldl (++) "\n" $ map showSvg datasets

svgScaleDataset :: (Fractional a, Ord a) => a -> a -> Dataset a -> Dataset a
svgScaleDataset width height d@(Dataset {datasetPoints = points}) =
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
    scale (x,y) = (width * (x - minX) / deltaX, height - height * (y - minY) / deltaY)

svgScaleChart :: (Fractional a, Ord a) => Chart a -> Chart a
svgScaleChart c@(Chart { chartWidth = w, chartHeight = h, chartData = d }) =
  c { chartData = fmap (svgScaleDataset (fromIntegral w) (fromIntegral h)) d }

instance (PrintfArg a, Fractional a, Ord a) => Svg (Chart a) where
  showSvg = svgChart . svgScaleChart
