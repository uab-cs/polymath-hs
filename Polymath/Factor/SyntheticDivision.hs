module Polymath.Factor.SyntheticDivision
  ( syntheticDiv
  , syntheticDiv'
  , syntheticRem
  , syntheticReduce
  ) where

import Polymath.Data(Operation, opSynthetic)
import Data.Ratio

syntheticDiv :: Integral a => [a] -> Ratio a -> [Ratio a]
syntheticDiv (p:ps) d = foldl (helper d) [p % 1] (map (%1) ps)

helper :: Integral a => Ratio a -> [Ratio a] -> Ratio a -> [Ratio a]
helper d list new = list ++ [(last list * d) + new]

syntheticDiv' :: Integral a => [a] -> Ratio a -> Operation
syntheticDiv' p d = opSynthetic p (syntheticDiv p d)

syntheticRem :: Integral a => [a] -> Ratio a -> Ratio a
syntheticRem p d = last $ syntheticDiv p d

syntheticReduce :: Integral a => [a] -> Ratio a -> [Ratio a]
syntheticReduce p d = init $ syntheticDiv p d
