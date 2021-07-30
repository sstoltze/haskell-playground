module Raycaster where

import Space
import Colour

import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (sortBy)

import Codec.Picture

data HitData = HitData { hitRay :: Ray Double
                       , hitIntersection :: Double
                       , hitColour :: Colour
                       , hitNormal :: Vector Double
                       } deriving Show

hitPoint :: HitData -> Position Double
hitPoint h = rayPoint (hitIntersection h) (hitRay h)

class SceneObject a where
  intersectRay :: Ray Double -> a -> Maybe HitData

instance SceneObject a => SceneObject [a] where
  intersectRay r objs = listToMaybe $ rayIntersections r objs

instance (SceneObject a, SceneObject b) => SceneObject (Either a b) where
  intersectRay r x = either (intersectRay r) (intersectRay r) x

instance SceneObject a => SceneObject (Maybe a) where
  intersectRay r x = x >>= intersectRay r

data BoundedObject a = BoundedObject { boundedObject :: a
                                     , xBound :: Maybe (Double, Double)
                                     , yBound :: Maybe (Double, Double)
                                     , zBound :: Maybe (Double, Double)
                                     }

instance SceneObject a => SceneObject (BoundedObject a) where
  intersectRay r a = if withinBounds hit then hit else Nothing
    where
      hit = intersectRay r (boundedObject a)
      withinBounds Nothing = False
      withinBounds (Just h) = let (Position x y z) = hitPoint h
        in maybe True (inInterval x) (xBound a)
        && maybe True (inInterval y) (yBound a)
        && maybe True (inInterval z) (zBound a)
      inInterval p (start, end) = start <= p && p <= end

type Material = (HitData -> Colour)

data TransmutedObject a = TransmutedObject { transmutedObject :: a
                                           , transmutedMaterial :: Material
                                           }

instance SceneObject a => SceneObject (TransmutedObject a) where
  intersectRay r x = intersectRay r (transmutedObject x) >>= \h -> return $ h { hitColour = transmutedMaterial x h }

rayIntersections :: SceneObject a => Ray Double -> [a] -> [HitData]
rayIntersections ray objects = hitSort $ mapMaybe (intersectRay ray) objects
  where
    hitSort = sortBy (\h1 h2 -> compare (hitIntersection h1) (hitIntersection h2))

data Resolution = Resolution { resolutionWidth :: Int
                             , resolutionHeight :: Int
                             }

data Camera = Camera { cameraPosition :: Position Double
                     , cameraDirection :: Vector Double
                     , cameraUp :: Vector Double
                     , cameraResolution :: Resolution
                     }

newtype Light = Light { lightPosition :: Position Double
                      }

data Scene a = Scene { sceneObject :: a
                     , sceneBackground :: Colour
                     , sceneCamera :: Camera
                     , sceneLights :: [Light]
                     }

cameraRay :: Camera -> Int -> Int -> Ray Double
cameraRay Camera { cameraPosition = p
                  , cameraUp = u
                  , cameraDirection = d
                  , cameraResolution = res
                  } x y =
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
    resolution = cameraResolution camera
    width = resolutionWidth resolution
    height = resolutionHeight resolution
    snap x y =
      case intersectRay (cameraRay camera x y) (sceneObject s) of
        Nothing -> colourPixel $ sceneBackground s
        Just h  -> colourPixel $ calculateColour h
    calculateColour hit = colourScale avgCosine (hitColour hit)
       where
         average [] = 0
         average xs = sum xs / fromIntegral (length xs)
         avgCosine = average $ map scaleFromLight $ sceneLights s
         scaleFromLight light =
           if isBlocked light
           then 0
           else dotProduct normalizedSurface $ normalizedLightVector light
         lightVector light = positionSubtract surfacePoint $ lightPosition light
         normalizedLightVector light = vectorNormalize $ lightVector light
         surfacePoint = hitPoint hit
         normalizedSurface = vectorNormalize $ hitNormal hit
         lightRay light = Ray { rayStart = lightPosition light
                              , rayDirection = lightVector light
                              }
         isBlocked light = maybe False (\h -> hitIntersection h < 0.9) (intersectRay (lightRay light) (sceneObject s))
