module Flock where

import           Data.Maybe    (catMaybes)
import           Entity
import           Space
import           System.Random (RandomGen)

type Flock = [Entity]

-- As a start, we want entities in the plane with speed 1.
entityNormalize :: Entity -> Entity
entityNormalize e =
  e
    { entityPosition = (entityPosition e) {pointZ = 0},
      entityDirection = vectorNormalize $ (entityDirection e) {vectorZ = 0}
    }

data FlockConfig = FlockConfig
  { flockConfigTooCloseDistance :: Double,
    flockConfigFlockingDistance :: Double
  }

tooCloseDistance :: Double
tooCloseDistance = 2.0

flockingDistance :: Double
flockingDistance = 50.0

defaultFlockConfig :: FlockConfig
defaultFlockConfig =
  FlockConfig
    { flockConfigTooCloseDistance = tooCloseDistance,
      flockConfigFlockingDistance = flockingDistance
    }

flockRandom :: (RandomGen g) => Int -> g -> (Flock, g)
flockRandom k g = flockRandom' k ([], g)
  where
    flockRandom' 0 r = r
    flockRandom' n (f, g1) = let (e, g2) = entityRandom g1 in flockRandom' (n -1) (e : f, g2)

flockUpdateWithConfig :: FlockConfig -> Flock -> Flock
flockUpdateWithConfig c f = flockUpdate' (zip f [0 ..])
  where
    flockUpdate' :: [(Entity, Int)] -> Flock
    flockUpdate' f' = map (updateEntity f') f'
    updateEntity f' (e, k) = entityMove $ entityNormalize $ e {entityDirection = vectorAverage (vecs f' (e, k))}
    vecs f' (e, k) = catMaybes [Just (entityDirection e), tooClose f' (e, k), flock f' (e, k)]
    tooClose f' (e, k) =
      let tooClose' = filter (closeness 0.0 (flockConfigTooCloseDistance c) k e) f'
       in fmap (vectorScale (-1)) $ vectorMaybeAverage $ map (entityDirection . fst) tooClose'
    flock f' (e, k) =
      let flock' = filter (closeness (flockConfigTooCloseDistance c) (flockConfigFlockingDistance c) k e) f'
       in vectorMaybeAverage $ map (entityDirection . fst) flock'
    closeness minDistance maxDistance k e (e', i) =
      let dist = entityDistance e e'
       in i /= k && dist >= minDistance && dist < maxDistance

flockUpdate :: Flock -> Flock
flockUpdate = flockUpdateWithConfig defaultFlockConfig

flockDistance :: Point Double -> Flock -> Double
flockDistance p f = minimum $ map (pointDistance p . entityPosition) f

planeFlockRandom :: (RandomGen g) => Int -> g -> (Flock, g)
planeFlockRandom k g = planeEntities $ flockRandom k g
  where
    planeEntities (f', g') = (fmap entityNormalize f', g')
