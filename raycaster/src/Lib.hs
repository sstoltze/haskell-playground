module Lib where

import Space
import Colour
import Raycaster
import Sphere
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

cameraRay :: Camera -> Int -> Int -> Ray
cameraRay (Camera { cameraPosition = p
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
      case rayIntersections (cameraRay camera x y) objects of
        []  -> colourPixel $ sceneBackground s
        h:_ -> colourPixel $ calculateColour h
    calculateColour hit = colourScale maxCosine (hitColour hit)
       where
         maxCosine = maximum $ map scaleFromLight $ sceneLights s
         scaleFromLight light = dotProduct normalizedSurface $ normalizedLightVector light
         normalizedLightVector light = vectorNormalize $ positionSubtract surfacePoint $ lightPosition light
         surfacePoint = hitPoint hit
         normalizedSurface = vectorNormalize $ hitNormal hit
