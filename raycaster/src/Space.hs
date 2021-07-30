module Space where

data Position a = Position { posX :: a
                           , posY :: a
                           , posZ :: a
                           } deriving (Show, Eq)

data Vector a = Vector { vecX :: a
                       , vecY :: a
                       , vecZ :: a
                       } deriving (Show, Eq)

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
   (v1x * v2x) + (v1y * v2y) + (v1z * v2z)

vectorScale :: (Num a) => a -> Vector a -> Vector a
vectorScale a (Vector x y z) = Vector (a * x) (a * y) (a * z)

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

positionAdd :: (Num a) => Position a -> Vector a -> Position a
positionAdd
  (Position  x  y  z)
  (Vector   vx vy vz) =
  Position (x + vx) (y + vy) (z + vz)

positionSubtract :: (Num a) => Position a -> Position a -> Vector a
positionSubtract
  (Position p1x p1y p1z)
  (Position p2x p2y p2z) =
   Vector    vx  vy  vz
  where
    vx = p1x - p2x
    vy = p1y - p2y
    vz = p1z - p2z

positionDistance :: (Floating a) => Position a -> Position a -> a
positionDistance p1 p2 = vectorLength $ positionSubtract p1 p2

data Ray a = Ray { rayStart :: Position a
                 , rayDirection :: Vector a
                 } deriving Show

rayPoint :: (Num a) => a -> Ray a-> Position a
rayPoint t (Ray start direction) = positionAdd start (vectorScale t direction)
