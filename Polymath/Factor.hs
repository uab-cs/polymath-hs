module Polymath.Factor
  ( factor'
  ) where

import Polymath.Data
import Polymath.Factor.Descarte
import Polymath.Factor.RationalZeros
import Polymath.Factor.SyntheticDivision

import Data.Ratio
import Data.Aeson

findZeros' :: Integral a =>  [a] -> a -> [Operation]
findZeros' p 0 = []
findZeros' p i =
  let stp1 = [possibleZeros' p]
      stp2 = [syntheticDiv' p (firstZero p)]
      rest = findZeros' (simplifyPoly p) (i - 1)
  in stp1 ++ stp2 ++ rest

factor' :: Integral a => [a] -> Response
factor' p =
  Response { steps = ruleOfSigns' p : findZeros' p (totalZeros p)
           , roots = map ratioToFrac (realZeros p)
           }

findZeros :: Integral a => [a] -> a -> [Ratio a]
findZeros p 0 = []
findZeros p i
  | p == simplifyPoly p = []
  | otherwise = firstZero p : findZeros (simplifyPoly p) (i - 1)

simplifyPoly :: Integral a => [a] -> [a]
simplifyPoly p
  | null (realZeros p) = p
  | otherwise = map ratioToInt (syntheticReduce p (firstZero p))
