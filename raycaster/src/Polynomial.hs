{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Polynomial where

import Data.Maybe (maybeToList)

data XYZ = X | Y | Z deriving (Show, Eq)
data T = T deriving (Show, Eq)

-- Polys in one var and three vars
data Polynomial a v = Const a
                    | Var v
                    | Sum (Polynomial a v) (Polynomial a v)
                    | Prod (Polynomial a v) (Polynomial a v)
                    deriving (Eq, Functor)

instance (Show a, Show v) => Show (Polynomial a v) where
  show (Const a) = show a
  show (Var v) = show v
  show (Sum p q) = "(" ++ show p ++ " + " ++ show q ++ ")"
  show (Prod p q) = "(" ++ show p ++ " * " ++ show q ++ ")"

instance (Num a) => Num (Polynomial a v) where
  p + q = Sum p q
  p * q = Prod p q
  negate q = Prod (Const (-1)) q
  fromInteger n = Const (fromInteger n)
  -- These do not really make sense ‾\_(ツ)_/‾
  abs p = p
  signum p = p

typeOfTerm :: Polynomial a v -> String
typeOfTerm (Const _) = "Const"
typeOfTerm (Var _) = "Var"
typeOfTerm (Sum _ _) = "Sum"
typeOfTerm (Prod _ _) = "Prod"

sameType :: Polynomial a v -> Polynomial b w -> Bool
sameType p q = typeOfTerm p == typeOfTerm q

testPoly :: Polynomial Double T
testPoly = Var T * (Const 2 + Var T + Const 10)

changeVariables :: (v -> Polynomial a w) -> Polynomial a v -> Polynomial a w
changeVariables newVar = \case
  Const a -> Const a
  Var v -> newVar v
  Sum p q -> Sum (changeVariables newVar p) (changeVariables newVar q)
  Prod p q -> Prod (changeVariables newVar p) (changeVariables newVar q)

derive :: (Num a) => (v -> Polynomial a v) -> Polynomial a v -> Polynomial a v
derive _ (Const _) = Const 0
derive f (Var v) = f v
derive f (Sum p q) = Sum (derive f p) (derive f q)
derive f (Prod p q) = Sum (Prod (derive f p) q) (Prod p (derive f q))

deriveCoefficients :: (Num a) => [a] -> [a]
deriveCoefficients p = zipWith (*) (drop 1 p) (map fromInteger [1..])

-- Various simplifications that can be done, mostly for debugging purposes
expand :: (Num a, Eq a, Eq v) => Polynomial a v -> Polynomial a v
expand (Sum (Const a) (Const b)) = Const (a + b)
expand (Sum p (Const 0)) = expand p
expand (Sum (Const c) p) = expand $ Sum p (Const c)
expand (Sum p (Sum q r)) = expand $ Sum (expand $ Sum p q) r
expand (Sum (Sum p (Const a)) (Const b)) = expand $ Sum p (Const (a + b))
expand (Sum (Sum p (Const a)) q) = expand $ Sum (expand $ Sum p q) (Const a)
expand (Sum p (Prod (-1) q)) =
  if expand p == expand q
  then Const 0
  else expand $ Sum (expand p) (expand $ Prod (-1) q)
expand (Sum p q) =
  let expandedP = expand p
      expandedQ = expand q
  in if sameType p expandedP && sameType q expandedQ
     then Sum expandedP expandedQ
     else expand (Sum expandedP expandedQ)
expand (Prod (Const 0) _) = Const 0
expand (Prod (Const 1) p) = expand p
expand (Prod (Const a) (Const b)) = Const (a * b)
expand (Prod p (Const c)) = expand $ Prod (Const c) (expand p)
expand (Prod p (Sum a b)) = expand $ Sum (Prod (expand p) (expand a)) (Prod (expand p) (expand b))
expand (Prod p (Prod q r)) = expand $ Prod (Prod (expand p) (expand q)) (expand r)
expand (Prod p q) =
  let expandedP = expand p
      expandedQ = expand q
  in if sameType p expandedP && sameType q expandedQ
     then Prod expandedP expandedQ
     else expand (Prod expandedP expandedQ)
expand (Const c) = Const c
expand (Var v) = Var v

evaluate :: (Num a) => (v -> a) -> Polynomial a v -> a
evaluate f = \case
  Const a -> a
  Var v -> f v
  Sum p q -> evaluate f p + evaluate f q
  Prod p q -> evaluate f p * evaluate f q

evaluateCoefficients :: (Num a) => [a] -> a -> a
evaluateCoefficients p x = sum $ zipWith (*) p $ iterate (*x) 1

degree :: Polynomial a T -> Int
degree (Const _) = 0
degree (Var _) = 1
degree (Sum p q) = max (degree p) (degree q)
degree (Prod p q) = degree p + degree q

newton :: (Fractional a) => (a -> Bool) -> Int -> Polynomial a T -> [a]
newton isRoot iterations p = newton' $ polynomiumCoefficients p
  where
    newton' [] = []
    newton' [_] = []
    newton' coeffs = maybeToList t ++ newton' (removeRoot t coeffs)
      where
        evalDerivedP = evaluateCoefficients $ deriveCoefficients coeffs
        evalP = evaluateCoefficients coeffs
        adjustStart i x0 =
          if i /= iterations && isRoot (evalDerivedP x0)
          then adjustStart (i+1) (x0 + fromRational 1/1000)
          else x0
        t = findRoot 0 (adjustStart 0 0)
        findRoot i x0
          | i == iterations = Nothing
          | isRoot (evalP x0) = Just x0
          | otherwise = findRoot (i+1) (x0 - evalP x0 / evalDerivedP x0)
    removeRoot Nothing _ = []
    removeRoot (Just x) coeffs = [ newCoeff i | i <- [0..len-2] ]
      where
        len = length coeffs
        newCoeff i = case i of
          k | k < 0        -> 0
          k | k == len - 2 -> coeffs !! (len-1)
          _                -> (coeffs !! (i+1)) + x * newCoeff (i+1)

polynomiumCoefficients :: (Num a) => Polynomial a T -> [a]
polynomiumCoefficients (Const a) = [a]
polynomiumCoefficients (Var T) = [0, 1]
polynomiumCoefficients (Sum p q) = coefficientSum pCoeffs qCoeffs
  where
    pCoeffs = polynomiumCoefficients p
    qCoeffs = polynomiumCoefficients q
    coefficientSum [] b = b
    coefficientSum a [] = a
    coefficientSum (a:as) (b:bs) = a+b : coefficientSum as bs
polynomiumCoefficients (Prod p q) = [prodCoefficient i | i <- [0..(pDeg + qDeg)]]
  where
    pCoeffs = polynomiumCoefficients p
    qCoeffs = polynomiumCoefficients q
    pDeg = degree p
    qDeg = degree q
    qCoeff j = if 0 <= j && j <= qDeg then qCoeffs !! j else 0
    pCoeff j = if 0 <= j && j <= pDeg then pCoeffs !! j else 0
    prodCoefficient i = sum [pCoeff j * qCoeff (i-j) | j <- [0..(pDeg + qDeg)]]

fromCoefficients :: (Num a) => [a] -> Polynomial a T
fromCoefficients = fromCoefficients' (Const 0) (Const 1)
  where
    fromCoefficients' acc _ [] = acc
    fromCoefficients' acc x (a:as) = fromCoefficients' (Sum (Prod (Const a) x) acc) (Prod (Var T) x) as
