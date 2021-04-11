{-# LANGUAGE LambdaCase #-}
module Variety where

import Colour
import Space
import Polynomial
import Raycaster

import Data.List (sort)
import Data.Maybe (listToMaybe)

-- Make a variety and instance Sceneobject
data Variety = Variety { varietyPolynomial :: Polynomial Double XYZ
                       , varietyColour :: Colour
                       } deriving Show

instance SceneObject Variety where
  intersectRay
    ray@(Ray { rayStart     = (Position rx ry rz)
             , rayDirection = rd@(Vector   dx dy dz) })
    var@(Variety { varietyPolynomial = p
                 , varietyColour = colour
                 }) =
    listToMaybe solutions >>= return . constructHit
    where
      newVariables = \case
        X -> Sum (Const rx) (Prod (Const dx) (Var T))
        Y -> Sum (Const ry) (Prod (Const dy) (Var T))
        Z -> Sum (Const rz) (Prod (Const dz) (Var T))
      pT = changeVariables newVariables  p
      solutions = filter (> 0) $ sort $ newton (\a -> abs a < 0.0001) 100 pT
      constructHit t = let normal = varietyNormal var $ rayPoint t ray
                           -- The "opposite" side of the variety is the inverse colour
                           hNormal = if dotProduct normal rd > 0 then normal else vectorScale (-1) normal
                           hColour = if dotProduct normal rd > 0 then colour else inverseColour colour
                       in HitData { hitRay = ray
                                  , hitIntersection = t
                                  , hitColour = hColour
                                  , hitNormal = hNormal
                                  }

spherePoly :: Position Double -> Double -> Polynomial Double XYZ
spherePoly (Position x0 y0 z0) r =
  (varX * varX) + (varY * varY) + (varZ * varZ) - (Const r) * (Const r)
  where
    varX = (Var X) - (Const x0)
    varY = (Var Y) - (Const y0)
    varZ = (Var Z) - (Const z0)

sphereVariety :: Position Double -> Double -> Colour -> Variety
sphereVariety centre radius colour = Variety { varietyPolynomial = spherePoly centre radius
                                             , varietyColour = colour
                                             }

jacobian :: Variety -> Vector (Polynomial Double XYZ)
jacobian (Variety p _) = Vector (deriveVar X p) (deriveVar Y p) (deriveVar Z p)
  where
    deriveVar v = derive (\var -> if var == v then Const 1 else Const 0)

evaluateVector :: (Num a) => Position a -> Vector (Polynomial a XYZ) -> Vector a
evaluateVector (Position x y z) (Vector px py pz) =
  Vector (evaluate sub px) (evaluate sub py) (evaluate sub pz)
  where
    sub = \case
      X -> x
      Y -> y
      Z -> z

varietyNormal :: Variety -> Position Double -> Vector Double
varietyNormal v p = vectorScale (-1) $ vectorNormalize $ evaluateVector p $ jacobian v

varietyChangeVariables :: (XYZ -> Polynomial Double XYZ) -> Variety -> Variety
varietyChangeVariables f v =
  v { varietyPolynomial = changeVariables f $ varietyPolynomial v }

varietyTranslate :: Vector Double -> Variety -> Variety
varietyTranslate (Vector dx dy dz) v = varietyChangeVariables translate v
  where
    translate X = (Var X) - (Const dx)
    translate Y = (Var Y) - (Const dy)
    translate Z = (Var Z) - (Const dz)

varietyRotate :: Vector Double -> Double -> Variety -> Variety
varietyRotate axis angle v = varietyChangeVariables rotate v
  where
    -- Rotation around Z-axis
    rotate X = (Const $ cos angle) * Var X - (Const $ sin angle) * Var Y
    rotate Y = (Const $ sin angle) * Var X + (Const $ cos angle) * Var Y
    rotate Z = Var Z

cylinderVariety :: Vector Double -> Double -> Colour -> Variety
cylinderVariety axis radius colour =
  varietyRotate axisOfRotation angleOfRotation $ Variety { varietyPolynomial = zCylinderPoly
                                                         , varietyColour = colour
                                                         }
  where
    axisOfRotation = crossProduct axis (Vector 0 0 1)
    angleOfRotation = acos $ dotProduct (vectorNormalize axis) (Vector 0 0 1)
    zCylinderPoly = (Var X) * (Var X) + (Var Y) * (Var Y) - (Const radius) * (Const radius)
