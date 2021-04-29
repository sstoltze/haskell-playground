module Lib where

import Space
import Colour
import Raycaster
import Sphere
import Codec.Picture
import Variety

simpleTest :: IO ()
simpleTest = savePngImage "/tmp/out-simple.png" img
  where
    img = picture simpleScene
    simpleScene = Scene { sceneObject = [ gradientSphere
                                        , lightnessSphere
                                        , saturationSphere
                                        , cylinder
                                        ] :: [Either (TransmutedObject Sphere) (TransmutedObject Variety)]
                        , sceneBackground = white
                        , sceneCamera = camera
                        , sceneLights = [ light1
                                        , light2
                                        , cameraLight
                                        ]
                        }
    black = createColour 0 0 0
    green = createColour 0 255 0
    red = createColour 255 0 0
    white = createColour 255 255 255
    blue = createColour 0 0 255
    sphereColour = createColour 255 0 0
    camera = Camera (Position (-10) (0) 0) (Vector (1) 0 0) (Vector 0 0 1) (Resolution 500 500)
    cameraLight = Light $ cameraPosition camera
    light1 = Light (Position (-7) (7) 0)
    light2 = Light (Position (-7) (-7) 0)
    gradientSphere = Left $ objectGradient $ Sphere (Position 0 0 0) 1 sphereColour
    lightnessSphere = Left $ objectLightness $ Sphere (Position 0 2 0) 1 sphereColour
    saturationSphere = Left $ objectSaturation $ Sphere (Position 0 (-2) 0) 1 sphereColour
    cylinder = Right $ objectGradient $ cylinderVariety (Vector 0 0 1) 10 green

objectGradient :: SceneObject a => a -> TransmutedObject a
objectGradient a = TransmutedObject { transmutedObject = a
                                    , transmutedMaterial = material
                                    }
  where
    material h = colourFromPoint (hitColour h) (hitNormal h)
    colourFromPoint c v = rotateColour (vectorToAngle $ vectorNormalize $ v { vecZ = 0 }) c
    vectorToAngle (Vector x y _) =
      let radians = if y < 0 then pi - acos x else acos x in
        360 * radians / pi

objectLightness :: SceneObject a => a -> TransmutedObject a
objectLightness a = TransmutedObject { transmutedObject = a
                                     , transmutedMaterial = material
                                     }
  where
    material h = colourFromPoint (hitColour h) (vectorNormalize $ vectorScale (-1) $ hitNormal h)
    colourFromPoint c (Vector _ _ z) = (colourToHsl c) { colourLightness = (z + 1) / 2 }

objectSaturation :: SceneObject a => a -> TransmutedObject a
objectSaturation a = TransmutedObject { transmutedObject = a
                                      , transmutedMaterial = material
                                      }
  where
    material h = colourFromPoint (hitColour h) (vectorNormalize $ vectorScale (-1) $ hitNormal h)
    colourFromPoint c (Vector _ _ z) = (colourToHsl c) { colourSaturation = (z + 1) / 2 }


testPic :: DynamicImage
testPic = picture testScene
  where
    grey = createColour 75 75 75
    red = createColour 255 0 0
    blue = createColour 0 0 255
    green = createColour 0 255 0
    purple = createColour 128 0 128
    redSphereVar   = Left $ Right $ objectGradient $ sphereVariety (Position 1 1 0) 1 red
    greenSphereVar = Left $ Right $ objectGradient $ sphereVariety (Position 0 2 0) 1 green
    blueSphereVar  = Left $ Right $ objectGradient $ sphereVariety (Position 0 1 1) 1 blue
    redSphere   = Right $ Right $ Sphere (Position 1 (-1) 0) 1 red
    greenSphere = Right $ Left $ BoundedObject { boundedObject = Sphere (Position 0 (-2) 0) 1 green
                                               , xBound = Just (-0.85, 0)
                                               , yBound = Just (-3, -1.3)
                                               , zBound = Nothing
                                               }
    blueSphere  = Right $ Left $ BoundedObject { boundedObject = Sphere (Position 0 (-1) 1) 1 blue
                                               , xBound = Nothing
                                               , yBound = Nothing
                                               , zBound = Just (1, 2)
                                               }
    cylinderVar = Left $ Right $ objectGradient $ varietyTranslate (Vector (4) (-4) 0) $ cylinderVariety (Vector 0 0 1) 2 purple
    surroundingCylinder = Left $ Left $ objectGradient $ cylinderVariety (Vector 0 0 1) (10) (inverseColour grey)
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
