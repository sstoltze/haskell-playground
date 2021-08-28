{-# LANGUAGE LambdaCase #-}
module Variety where

import           Colour
import           Polynomial
import           Raycaster
import           Space

import           Data.Functor ((<&>))
import           Data.List    (sort)
import           Data.Maybe   (listToMaybe)

data Variety = Variety { varietyPolynomial :: Polynomial Double XYZ
                       , varietyColour     :: Colour
                       } deriving Show

instance SceneObject Variety where
  intersectRay
    ray@Ray { rayStart     = (Position  rx ry rz)
            , rayDirection = rd@(Vector dx dy dz) }
    var@Variety { varietyPolynomial = p
                , varietyColour = colour
                 } =
    listToMaybe solutions <&> constructHit
    where
      newVariables = \case
        X -> Const rx + Const dx * Var T
        Y -> Const ry + Const dy * Var T
        Z -> Const rz + Const dz * Var T
      pT = expand $ changeVariables newVariables p
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
  (varX * varX) + (varY * varY) + (varZ * varZ) - Const r * Const r
  where
    varX = Var X - Const x0
    varY = Var Y - Const y0
    varZ = Var Z - Const z0

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
varietyTranslate (Vector dx dy dz) = varietyChangeVariables translate
  where
    translate X = Var X - Const dx
    translate Y = Var Y - Const dy
    translate Z = Var Z - Const dz

varietyExpand :: Variety -> Variety
varietyExpand v = v { varietyPolynomial = expand $ varietyPolynomial v }

varietyRotate :: Vector Double -> Double -> Variety -> Variety
varietyRotate axis angle = rotateFromZ . rotateAroundZ angle . rotateToZ
  where
    normalAxis@(Vector nx ny nz) = vectorNormalize axis
    -- Axis projected to YZ plane
    (Vector _ sinA cosA) = if vectorParallel normalAxis (Vector 1 0 0)
      then Vector 0 0 1
      else vectorNormalize (normalAxis { vecX = 0 })
    -- The relevant coordinates from the rotation to the YZ plane
    (sinB, cosB) = (-nx, sinA * ny + cosA * nz)
    -- Rotation around Z-axis
    rotateAroundZ a = varietyChangeVariables (rotateZ a)
    rotateZ a X = Const (cos a) * Var X - Const (sin a) * Var Y
    rotateZ a Y = Const (sin a) * Var X + Const (cos a) * Var Y
    rotateZ _ Z = Var Z
    rotateToZ = rotateAroundY cosB sinB . rotateAroundX cosA sinA
    rotateAroundX c s = varietyChangeVariables (rotateVarToXZ c s)
    rotateVarToXZ _ _ X = Var X
    rotateVarToXZ c s Y = Const c * Var Y - Const s * Var Z
    rotateVarToXZ c s Z = Const s * Var Y + Const c * Var Z
    rotateAroundY c s = varietyChangeVariables (rotateVarToZ c s)
    rotateVarToZ c s X = Const c * Var X - Const s * Var Z
    rotateVarToZ _ _ Y = Var Y
    rotateVarToZ c s Z = Const s * Var X + Const c * Var Z
    rotateFromZ = rotateAroundX cosA (-sinA) . rotateAroundY cosB (-sinB)

cylinderVariety :: Position Double -> Vector Double -> Double -> Colour -> Variety
cylinderVariety point axis radius colour =
  varietyTranslate (positionSubtract point (Position 0 0 0))
  $ varietyRotate axisOfRotation (-angleOfRotation)
  $ Variety { varietyPolynomial = zCylinderPoly
            , varietyColour = colour
            }
  where
    zAxis = Vector 0 0 1
    axisOfRotation = crossProduct axis zAxis
    angleOfRotation = vectorAngle axis zAxis
    zCylinderPoly = Var X * Var X + Var Y * Var Y - Const radius * Const radius

planeVariety :: Position Double -> Vector Double -> Colour -> Variety
planeVariety point normal colour =
  varietyTranslate (positionSubtract point (Position 0 0 0))
  $ varietyRotate axisOfRotation (-angleOfRotation)
  $ Variety { varietyPolynomial = Var Z
            , varietyColour = colour
            }
  where
    zAxis = Vector 0 0 1
    axisOfRotation = if vectorParallel normal zAxis then zAxis else crossProduct normal zAxis
    angleOfRotation = vectorAngle normal zAxis
