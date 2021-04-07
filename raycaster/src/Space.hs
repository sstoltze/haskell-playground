module Space where

data Position = Position { posX :: Double
                         , posY :: Double
                         , posZ :: Double
                         } deriving (Show, Eq)

data Vector = Vector { vecX :: Double
                     , vecY :: Double
                     , vecZ :: Double
                     } deriving (Show, Eq)

crossProduct :: Vector -> Vector -> Vector
crossProduct
  (Vector v1x v1y v1z)
  (Vector v2x v2y v2z) =
   Vector v3x v3y v3z
  where
    v3x = v1y * v2z - v1z * v2y
    v3y = v1z * v2x - v1x * v2z
    v3z = v1x * v2y - v1y * v2x

dotProduct :: Vector -> Vector -> Double
dotProduct
  (Vector v1x v1y v1z)
  (Vector v2x v2y v2z) =
   (v1x * v2x) + (v1y * v2y) + (v1z * v2z)

vectorScale :: Double -> Vector -> Vector
vectorScale a (Vector x y z) = Vector (a * x) (a * y) (a * z)

vectorLength :: Vector -> Double
vectorLength (Vector x y z) = sqrt $ square x + square y + square z
  where
    square a = a ^ (2 :: Int)

vectorNormalize :: Vector -> Vector
vectorNormalize v =
  if len == 0
  then v
  else vectorScale (1/len) v
  where
    len = vectorLength v

positionAdd :: Position -> Vector -> Position
positionAdd
  (Position  x  y  z)
  (Vector   vx vy vz) =
  Position (x + vx) (y + vy) (z + vz)

positionSubtract :: Position -> Position -> Vector
positionSubtract
  (Position p1x p1y p1z)
  (Position p2x p2y p2z) =
   Vector    vx  vy  vz
  where
    vx = p1x - p2x
    vy = p1y - p2y
    vz = p1z - p2z

positionDistance :: Position -> Position -> Double
positionDistance p1 p2 = vectorLength $ positionSubtract p1 p2
