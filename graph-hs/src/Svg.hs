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

instance PrintfArg a => Svg (Dataset a) where
  showSvg d | datasetType d == Line = svgLine d
            | datasetType d == Circles = svgCircle d

instance (PrintfArg a, Fractional a, Ord a) => Svg (Chart a) where
  showSvg = svgChart . svgScaleChart

svgPoint :: (PrintfArg a) => Point a -> String
svgPoint p = printf "%.2f" (pointX p) ++ "," ++ printf "%.2f" (pointY p)

svgLine :: (PrintfArg a) => Dataset a -> String
svgLine d = "<polyline stroke='" ++ colour ++ "' stroke-width='1.5' fill='none' points='" ++ svgPoints ++ "'/>"
  where
    colour = hexColour $ datasetColour d
    svgPoints = foldr (\p r -> svgPoint p ++ " " ++ r) "" $ datasetPoints d

svgCircle :: (PrintfArg a) => Dataset a -> String
svgCircle d = foldr (++) "\n" svgCircles
  where
    colour = hexColour $ datasetColour d
    pointToSvgCircle p =
      printf "<circle cx='%.2f' cy='%.2f' r='1.2' fill='%s' stroke='%s' />" (pointX p) (pointY p) colour colour
    svgCircles = fmap pointToSvgCircle $ datasetPoints d

svgChart :: (PrintfArg a) => Chart a -> String
svgChart c = svgLines
  where
    datasets = chartData c
    svgLines = foldr (++) "\n" $ map showSvg datasets

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
    scale p = (width * ((pointX p) - minX) / deltaX, height - height * ((pointY p) - minY) / deltaY)

svgScaleChart :: (Fractional a, Ord a) => Chart a -> Chart a
svgScaleChart c@(Chart { chartWidth = w, chartHeight = h, chartData = d }) =
  c { chartData = fmap (svgScaleDataset (fromIntegral w) (fromIntegral h)) d }
