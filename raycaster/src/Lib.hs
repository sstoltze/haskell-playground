module Lib where

import Space
import Colour
import Raycaster
import Sphere
import Codec.Picture
import Variety

testPic :: DynamicImage
testPic = picture testScene
  where
    grey = createColour 75 75 75
    red = createColour 255 0 0
    blue = createColour 0 0 255
    green = createColour 0 255 0
    purple = createColour 128 0 128
    redSphereVar   = Left $ sphereVariety (Position 1 1 0) 1 red
    greenSphereVar = Left $ sphereVariety (Position 0 2 0) 1 green
    blueSphereVar  = Left $ sphereVariety (Position 0 1 1) 1 blue
    redSphere   = Right $ Sphere (Position 1 (-1) 0) 1 red
    greenSphere = Right $ Sphere (Position 0 (-2) 0) 1 green
    blueSphere  = Right $ Sphere (Position 0 (-1) 1) 1 blue
    cylinderVar = Left $ varietyTranslate (Vector (4) (-4) 0) $ cylinderVariety (Vector 0 0 1) 2 purple
    surroundingCylinder = Left $ cylinderVariety (Vector 0 0 1) (10) (inverseColour grey)
    testCamera = Camera (Position (-10) (0) 0) (Vector (1) 0 0) (Vector 0 0 1) (Resolution 500 500)
    testLightCamera = Light (cameraPosition testCamera)
    testLight = Light (Position (-9) (-1) (1))
    -- testLight2 = Light (Position (-2) (-1) 1)
    --  testLightBelow = Light (Position 0 0 (-10))
    -- testLightAbove = Light (Position 0 0 10)
    -- testLightLeft = Light (Position 0 10 0)
    testScene = Scene { sceneObject = [ redSphere
                                      , blueSphere
                                      , greenSphere
                                      , redSphereVar
                                      , blueSphereVar
                                      , greenSphereVar
                                      , cylinderVar
                                      , surroundingCylinder
                                      ]
                      , sceneBackground = grey
                      , sceneCamera = testCamera
                      , sceneLights = [ testLightCamera
                                      , testLight
                                      -- , testLight2
                                      ]
                      }

runTest :: IO ()
runTest = savePngImage "/tmp/out.png" testPic
