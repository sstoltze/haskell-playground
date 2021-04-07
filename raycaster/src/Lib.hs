module Lib where

import Space
import Colour
import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Codec.Picture

testPic :: DynamicImage
testPic = picture testScene
  where
    grey = createColour 75 75 75
    red = createColour 255 0 0
    blue = createColour 0 0 255
    green = createColour 0 255 0
    redSphere   = Sphere (Position 1 0 0) 1 red
    blueSphere  = Sphere (Position 0 0 1) 1 blue
    greenSphere = Sphere (Position 0 1 0) 1 green
    testCamera = Camera (Position (-10) 0 0) (Vector (1) 0 0) (Vector 0 0 1) (Resolution 500 500)
    testLightCamera = Light (Position (-10) (0) (0))
    testLight = Light (Position (-5) (-5) (-5))
    -- testLightBelow = Light (Position 0 0 (-10))
    -- testLightAbove = Light (Position 0 0 10)
    -- testLightLeft = Light (Position 0 10 0)
    testScene = Scene { sceneObjects = [redSphere, blueSphere, greenSphere]
                      , sceneBackground = grey
                      , sceneCamera = testCamera
                      , sceneLights = [testLightCamera, testLight]
                      }

runTest :: IO ()
runTest = savePngImage "/tmp/out.png" testPic

data Ray = Ray { rayStart :: Position
               , rayDirection :: Vector
               } deriving Show

rayPoint :: Double -> Ray -> Position
rayPoint t (Ray start direction) = positionAdd start (vectorScale t direction)

rayIntersections :: SceneObject a => Ray -> [a] -> [HitData]
rayIntersections ray objects =
      sortBy (\h1 h2 -> compare (hitIntersection h1) (hitIntersection h2)) $ mapMaybe (intersectRay ray) objects

data Resolution = Resolution { resolutionWidth :: Int
                             , resolutionHeight :: Int
                             }

data Camera = Camera { cameraPosition :: Position
                     , cameraDirection :: Vector
                     , cameraUp :: Vector
                     , cameraResolution :: Resolution
                     }

data HitData = HitData { hitRay :: Ray
                       , hitIntersection :: Double
                       , hitColour :: Colour
                       , hitNormal :: Vector
                       } deriving Show

hitPoint :: HitData -> Position
hitPoint h = rayPoint (hitIntersection h) (hitRay h)

class SceneObject a where
  intersectRay :: Ray -> a -> Maybe HitData

data Sphere = Sphere { sphereCentre :: Position
                     , sphereRadius :: Double
                     , sphereColour :: Colour
                     }

instance SceneObject Sphere where
  intersectRay
    ray@(Ray { rayStart     = (Position rx ry rz)
             , rayDirection = (Vector   dx dy dz) })
    (Sphere { sphereCentre = centre@(Position sx sy sz)
            , sphereRadius = r
            , sphereColour = colour }) =
    let a = dx*dx+dy*dy+dz*dz
        b = 2 * dx * (rx - sx) + 2 * dy * (ry - sy) + 2 * dz * (rz - sz)
        c = sx*sx + sy*sy + sz*sz - r*r + rx*rx + ry*ry + rz*rz - 2 * (rx*sx + ry*sy + rz*sz)
        d = sqrt $ b * b - 4 * a * c
        t1 = (-b + d)/(2 * a)
        t2 = (-b - d)/(2 * a)
        intersection = if t1 * t2 < 0 then max t1 t2 else min t1 t2
    in
      if isNaN d || (t1 < 0 && t2 < 0)
      then Nothing
      else Just $ HitData { hitRay = ray
                          , hitIntersection = intersection
                          , hitColour = colour
                          , hitNormal = positionSubtract centre (rayPoint intersection ray)
                          }

data Light = Light { lightPosition :: Position
                   }

data Scene a = Scene { sceneObjects :: [a]
                     , sceneBackground :: Colour
                     , sceneCamera :: Camera
                     , sceneLights :: [Light]
                     }

makeRay :: Camera -> Int -> Int -> Ray
makeRay (Camera { cameraPosition = p
                , cameraUp = u
                , cameraDirection = d
                , cameraResolution = res
                }) x y =
  Ray { rayStart = p
      , rayDirection = vectorNormalize $ positionSubtract p1 p
      }
  where
    height = resolutionHeight res
    width  = resolutionWidth res
    yScale = (fromIntegral height / 2 - fromIntegral y) / fromIntegral height
    xScale = (fromIntegral width  / 2 - fromIntegral x) / fromIntegral width
    normalizedForward = vectorNormalize d
    normalizedUp = vectorNormalize u
    scaledUp = vectorScale yScale normalizedUp
    scaledLeft = vectorScale xScale $ crossProduct normalizedUp normalizedForward
    p1 = positionAdd (positionAdd (positionAdd p normalizedForward) scaledUp) scaledLeft

picture :: (SceneObject a) => Scene a -> DynamicImage
picture s = ImageRGB8 (generateImage snap width height)
  where
    camera = sceneCamera s
    objects = sceneObjects s
    resolution = cameraResolution camera
    width = resolutionWidth resolution
    height = resolutionHeight resolution
    snap x y =
      case rayIntersections (makeRay camera x y) objects of
        []  -> colourPixel $ sceneBackground s
        h:_ -> colourPixel $ calculateColour h
    calculateColour hit = colourScale maxCosine (hitColour hit)
       where
         maxCosine = maximum $ map scaleFromLight $ sceneLights s
         scaleFromLight light = dotProduct normalizedSurface $ normalizedLightVector light
         normalizedLightVector light = vectorNormalize $ positionSubtract surfacePoint $ lightPosition light
         surfacePoint = hitPoint hit
         normalizedSurface = vectorNormalize $ hitNormal hit
