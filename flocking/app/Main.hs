module Main where

import           Codec.Picture
import           Data.Functor       ((<&>))
import           Data.Maybe         (listToMaybe)
import           Entity
import           Flock
import           Space
import           System.Environment (getArgs)
import           System.Random

data AppConfig = AppConfig
  { numberOfRuns     :: Int,
    numberOfEntities :: Int,
    pictureWidth     :: Int,
    pictureHeight    :: Int,
    imageDirectory   :: String
  }

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { numberOfRuns = 50,
      numberOfEntities = 20,
      pictureWidth = 500,
      pictureHeight = 500,
      imageDirectory = "/tmp/"
    }

flockDraw :: AppConfig -> Flock -> DynamicImage
flockDraw config f = ImageRGB8 (generateImage draw width height)
  where
    width = pictureWidth config
    height = pictureHeight config
    f' = map (\e -> e {entityPosition = (entityPosition e) {pointZ = 0}}) f
    draw x y =
      let p = Point ((fromIntegral x - fromIntegral width / 2) / 10) ((fromIntegral y - fromIntegral height / 2) / 10) 0
       in if flockDistance p f' < 0.5
            then PixelRGB8 200 200 200
            else PixelRGB8 50 50 50

buildFileName :: AppConfig -> Int -> String
buildFileName AppConfig {imageDirectory = dir} k = correctDir ++ "flock" ++ show k ++ ".png"
  where
    correctDir = case safeLast dir of
      Just '/'  -> dir
      Just '\\' -> dir
      _         -> dir ++ "/"
    safeLast = listToMaybe . reverse

flockRun :: AppConfig -> Flock -> IO Flock
flockRun config = flockRun' 0
  where
    iterations = numberOfRuns config
    flockRun' k f =
      if k == iterations
        then return f
        else do
          let fileName = buildFileName config k
          savePngImage fileName (flockDraw config f)
          putStrLn fileName
          flockRun' (k + 1) (flockUpdate f)

main :: IO ()
main = do
  config <- getArgs <&> parseArgs
  g <- getStdGen
  let (f, _) = planeFlockRandom (numberOfEntities config) g
  _ <- flockRun config f
  return ()

parseArgs :: [String] -> AppConfig
parseArgs as = parseArgs' as defaultAppConfig
  where
    parseArgs' [] config = config
    parseArgs' [_] config = config
    parseArgs' ("--count" : n : as') config = parseArgs' as' $ config {numberOfEntities = read n}
    parseArgs' ("-c" : n : as') config = parseArgs' as' $ config {numberOfEntities = read n}
    parseArgs' ("--iterations" : n : as') config = parseArgs' as' $ config {numberOfRuns = read n}
    parseArgs' ("-i" : n : as') config = parseArgs' as' $ config {numberOfRuns = read n}
    parseArgs' ("--height" : n : as') config = parseArgs' as' $ config {pictureHeight = read n}
    parseArgs' ("-h" : n : as') config = parseArgs' as' $ config {pictureHeight = read n}
    parseArgs' ("--width" : n : as') config = parseArgs' as' $ config {pictureWidth = read n}
    parseArgs' ("-w" : n : as') config = parseArgs' as' $ config {pictureWidth = read n}
    parseArgs' ("--output-dir" : d : as') config = parseArgs' as' $ config {imageDirectory = d}
    parseArgs' _ config = config
