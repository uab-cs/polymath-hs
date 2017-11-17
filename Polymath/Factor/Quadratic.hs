module Polymath.Factor.Quadratic
  ( quadratic
  ) where

import Data.Ratio

quadratic :: Integral a -> [a] -> [Ratio a]
quadratic (a:b:c:d) = let disc = sqrt( b^2 - (4 * a * c) )
                      in [disc % 1]
quadratic _ = []
