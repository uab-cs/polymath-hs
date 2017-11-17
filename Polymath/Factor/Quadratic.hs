module Polymath.Factor.Quadratic
  ( quadratic
  ) where

import Data.Ratio

quadratic :: (Fractional a, Integral a) => [a] -> [Ratio a]
quadratic (a:b:c:_) = let a' = fromIntegral a
                          b' = fromIntegral b
                          c' = fromIntegral c
                      in let disc = sqrt(b'^2 - (4 * a' * c'))
                             div  = (/) (2 * a')
                             nb   = negate b'
                         in [div (nb + disc) % 1, div (nb - disc) % 1]
quadratic _ = []
