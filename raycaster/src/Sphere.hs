module Sphere where

import Colour
import Space
import Raycaster

data Sphere = Sphere { sphereCentre :: Position Double
                     , sphereRadius :: Double
                     , sphereColour :: Colour
                     }

instance SceneObject Sphere where
  intersectRay
    ray@Ray { rayStart     = (Position rx ry rz)
            , rayDirection = (Vector   dx dy dz) }
    Sphere { sphereCentre = centre@(Position sx sy sz)
           , sphereRadius = r
           , sphereColour = colour } =
    let a = dx*dx+dy*dy+dz*dz
        b = 2 * dx * (rx - sx) + 2 * dy * (ry - sy) + 2 * dz * (rz - sz)
        c = sx*sx + sy*sy + sz*sz - r*r + rx*rx + ry*ry + rz*rz - 2 * (rx*sx + ry*sy + rz*sz)
        d = sqrt $ b * b - 4 * a * c
        t1 = (-b + d)/(2 * a)
        t2 = (-b - d)/(2 * a)
        intersection = if t1 * t2 < 0 then max t1 t2 else min t1 t2
    in
      if isNaN d || (t1 < 0 && t2 < 0)
      then Nothing
      else Just $ HitData { hitRay = ray
                          , hitIntersection = intersection
                          , hitColour = colour
                          , hitNormal = positionSubtract centre (rayPoint intersection ray)
                          }
