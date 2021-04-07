module Raycaster where

import Space
import Colour
import Data.Maybe (mapMaybe)
import Data.List (sortBy)

data HitData = HitData { hitRay :: Ray
                       , hitIntersection :: Double
                       , hitColour :: Colour
                       , hitNormal :: Vector
                       } deriving Show

hitPoint :: HitData -> Position
hitPoint h = rayPoint (hitIntersection h) (hitRay h)

class SceneObject a where
  intersectRay :: Ray -> a -> Maybe HitData

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

data Light = Light { lightPosition :: Position
                   }

data Scene a = Scene { sceneObjects :: [a]
                     , sceneBackground :: Colour
                     , sceneCamera :: Camera
                     , sceneLights :: [Light]
                     }
