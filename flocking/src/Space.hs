module Space where

import           System.Random (Random, RandomGen, randomR)

data Point a = Point { pointX :: a
                     , pointY :: a
                     , pointZ :: a
                     } deriving (Show, Eq)

data Vector a = Vector { vectorX :: a
                       , vectorY :: a
                       , vectorZ :: a
                       } deriving (Show, Eq)

vectorScale :: (Num a) => a -> Vector a -> Vector a
vectorScale c (Vector x y z) = Vector (c * x) (c * y) (c * z)

pointAdd :: (Num a) => Point a -> Vector a -> Point a
pointAdd (Point x y z) (Vector dx dy dz) = Point (x + dx) (y + dy) (z + dz)

vectorBin :: (a -> a -> b) -> Vector a -> Vector a -> Vector b
vectorBin f (Vector x y z) (Vector x' y' z') = Vector (f x x') (f y y') (f z z')

vectorAdd :: (Num a) => Vector a -> Vector a -> Vector a
vectorAdd = vectorBin (+)

vectorSubtract :: (Num a) => Vector a -> Vector a -> Vector a
vectorSubtract = vectorBin (-)

crossProduct :: (Num a) => Vector a -> Vector a -> Vector a
crossProduct
  (Vector v1x v1y v1z)
  (Vector v2x v2y v2z) =
   Vector v3x v3y v3z
  where
    v3x = v1y * v2z - v1z * v2y
    v3y = v1z * v2x - v1x * v2z
    v3z = v1x * v2y - v1y * v2x

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct
  (Vector v1x v1y v1z)
  (Vector v2x v2y v2z) =
   v1x * v2x + v1y * v2y + v1z * v2z

vectorLength :: (Floating a) => Vector a -> a
vectorLength (Vector x y z) = sqrt $ square x + square y + square z
  where
    square a = a ^ (2 :: Int)

vectorNormalize :: (Floating a, Eq a) => Vector a -> Vector a
vectorNormalize v =
  if len == 0
  then v
  else vectorScale (1/len) v
  where
    len = vectorLength v

vectorParallel :: (Eq a, Floating a) => Vector a -> Vector a -> Bool
vectorParallel v w = vectorLength (crossProduct v w) == 0

vectorOrthogonal :: (Eq a, Num a) => Vector a -> Vector a -> Bool
vectorOrthogonal v w = dotProduct v w == 0

vectorAngle :: (Floating a, Eq a) => Vector a -> Vector a -> a
vectorAngle v w = acos c
  where
    c = dotProduct (vectorNormalize v) (vectorNormalize w)

pointSubtract :: (Num a) => Point a -> Point a -> Vector a
pointSubtract
  (Point p1x p1y p1z)
  (Point p2x p2y p2z) =
   Vector (p1x - p2x) (p1y - p2y) (p1z - p2z)

pointDistance :: (Floating a) => Point a -> Point a -> a
pointDistance p1 p2 = vectorLength $ pointSubtract p1 p2

vectorAverage :: (Fractional a) => [Vector a] -> Vector a
vectorAverage vs = div' $ foldr1 avg (zip vs (repeat 1))
  where
    avg (v1, k1) (v2, k2) = (vectorAdd v1 v2, k1 + k2)
    div' (v, n) = vectorScale (1/n) v

vectorMaybeAverage :: (Fractional a) => [Vector a] -> Maybe (Vector a)
vectorMaybeAverage [] = Nothing
vectorMaybeAverage vs = Just $ vectorAverage vs

pointRandom :: (RandomGen g, Random a) => (a, a) -> g -> (Point a, g)
pointRandom hiLo g = (Point pX pY pZ, g3)
  where
    (pX, g1) = randomR hiLo g
    (pY, g2) = randomR hiLo g1
    (pZ, g3) = randomR hiLo g2

vectorRandom :: (RandomGen g, Random a) => (a, a) -> g -> (Vector a, g)
vectorRandom hiLo g = (Vector vecX vecY vecZ, g3)
  where
    (vecX, g1) = randomR hiLo g
    (vecY, g2) = randomR hiLo g1
    (vecZ, g3) = randomR hiLo g2
