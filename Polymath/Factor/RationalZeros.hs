module Polymath.Factor.RationalZeros
  ( possibleZeros
  , possibleZeros'
  , firstZero
  , realZeros
  ) where

import Polymath.Data(Operation, opZeros)
import Polymath.Factor.SyntheticDivision(syntheticRem)
import Data.Ratio
import Data.List(nub)

isFactorOf :: Integral a => a -> a -> Bool
isFactorOf x n = n `mod` x == 0

factorList n = let an = abs n
               in  an : filter (`isFactorOf` an) [1 .. an `div` 2]

possibleZeros' :: Integral a => [a] -> Operation
possibleZeros' p = opZeros (possibleZeros p) (realZeros p)

possibleZeros :: Integral a => [a] -> [Ratio a]
possibleZeros poly =
  if null poly
  then []
  else let p = factorList $ head poly
           q = factorList $ last poly
       in nub $ concatMap (\e -> concatMap (\f -> [f % e, negate (f % e)]) q) p

realZeros :: Integral a => [a] -> [Ratio a]
realZeros p = filter (zeroRem p) (possibleZeros p)

firstZero :: Integral a => [a] -> Ratio a
firstZero p = if null p
              then 0 % 1
              else head $ realZeros p

zeroRem :: Integral a => [a] -> Ratio a -> Bool
zeroRem p d = (0 % 1) == syntheticRem p d
