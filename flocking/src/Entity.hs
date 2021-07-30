module Entity where

import System.Random (RandomGen)

import Space

data Entity = Entity { entityPosition :: Point Double
                     , entityDirection :: Vector Double
                     } deriving (Show)

entityMove :: Entity -> Entity
entityMove e =
  e { entityPosition = pointAdd (entityPosition e) (entityDirection e) }

entityDistance :: Entity -> Entity -> Double
entityDistance e1 e2 = pointDistance (entityPosition e1) (entityPosition e2)

entityRandom :: (RandomGen g) => g -> (Entity, g)
entityRandom g = (e, g2)
  where
    (pos, g1) = pointRandom (-5.0, 5.0) g
    (dir, g2) = vectorRandom (-5.0, 5.0) g1
    e = Entity { entityPosition = pos
               , entityDirection = dir
               }
