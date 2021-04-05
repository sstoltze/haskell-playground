module Lib where

import Space
import Colour
import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Codec.Picture

testPic :: DynamicImage
testPic = picture testScene
  where
    red = createColour 255 0 0
    black = createColour 0 0 0
    blue = createColour 0 0 255
    green = createColour 0 255 0
    redSphere   = Sphere (Position 1 0 0) 1 red
    blueSphere  = Sphere (Position 0 1 0) 1 blue
    greenSphere = Sphere (Position 0 0 1) 1 green
    testCamera = Camera (Position (10) 0 0) (Vector (-1) 0 0) (Vector 0 0 1) (Resolution 500 500)
    testLight = Light (Position (-10) (-10) (-10))
    testScene = Scene { sceneObjects = [redSphere, blueSphere, greenSphere]
                      , sceneBackground = black
                      , sceneCamera = testCamera
                      , sceneLight = testLight
                      }

runTest :: IO ()
runTest = savePngImage "/tmp/out.png" testPic

data Ray = Ray { rayStart :: Position
               , rayDirection :: Vector
               } deriving Show

rayPoint :: Double -> Ray -> Position
rayPoint t (Ray start direction) = positionAdd start (vectorScale t direction)

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
    (Sphere { sphereCentre = (Position sx sy sz)
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
                          , hitNormal = positionSubtract (Position 0 0 0) (rayPoint intersection ray)
                          }

data Light = Light { lightPosition :: Position
                   }

data Scene a = Scene { sceneObjects :: [a]
                     , sceneBackground :: Colour
                     , sceneCamera :: Camera
                     , sceneLight :: Light
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
    yScale = (fromIntegral y - fromIntegral height / 2) / fromIntegral height
    xScale = (fromIntegral x - fromIntegral width  / 2) / fromIntegral width
    normalizedForward = vectorNormalize d
    normalizedUp = vectorNormalize u
    scaledUp = vectorScale yScale normalizedUp
    scaledLeft = vectorScale xScale $ crossProduct normalizedForward normalizedUp
    p1 = positionAdd (positionAdd (positionAdd p normalizedForward) scaledUp) scaledLeft

picture :: (SceneObject a) => Scene a -> DynamicImage
picture s = ImageRGB8 (generateImage snap width height)
  where
    camera = sceneCamera s
    resolution = cameraResolution camera
    width = resolutionWidth resolution
    height = resolutionHeight resolution
    snap x y =
      case intersections (makeRay camera x y) of
        [] -> colourPixel $ sceneBackground s
        h:_ -> colourPixel $ calculateColour h
    intersections ray =
      sortBy (\h1 h2 -> compare (hitIntersection h1) (hitIntersection h2)) $ mapMaybe (intersectRay ray) $ sceneObjects s
    calculateColour hit = colourScale c (hitColour hit)
       where
         light = lightPosition $ sceneLight s
         c = cos (dotProduct normalizedSurface normalizedLight)
         normalizedLight = vectorNormalize $ positionSubtract surfacePoint light
         surfacePoint = rayPoint (hitIntersection hit) (hitRay hit)
         normalizedSurface = vectorNormalize $ hitNormal hit
