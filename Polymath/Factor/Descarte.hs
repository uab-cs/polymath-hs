module Polymath.Factor.Descarte
  ( ruleOfSigns'
  , totalZeros
  ) where

import Polymath.Data(Operation, opDescarte)

ruleOfSigns' :: Integral a => [a] -> Operation
ruleOfSigns' p = let bound = ruleOfSigns p
                 in uncurry opDescarte bound

ruleOfSigns :: Integral a => [a] -> (a, a)
ruleOfSigns p = (signChanges 0 p, signChanges 0 (mapOdd (*(-1)) p []))

totalZeros :: Integral a => [a] -> a
totalZeros p = let rOs = ruleOfSigns p
               in uncurry (+) rOs

signChanges :: Integral a => a -> [a] -> a
signChanges i [x]    = i
signChanges i (x:y:xs) = if y /= 0
                         then if diffSign x y
                              then signChanges (i + 1) (y:xs)
                              else signChanges i (y:xs)
                         else signChanges i (x:xs)

diffSign :: Integral a => a -> a -> Bool
diffSign x y = (x < 0 && y > 0) || (x > 0 && y < 0)

mapOdd :: (a -> a) -> [a] -> [a] -> [a]
mapOdd f [] ys = ys
mapOdd f (x:xs) ys = if evenList (x:xs)
                     then mapOdd f xs (x:ys)
                     else mapOdd f xs (f x : ys)

evenList :: [a] -> Bool
evenList xs = 0 == fromIntegral (length xs `mod` 2)
