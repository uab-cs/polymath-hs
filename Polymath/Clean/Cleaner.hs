module Polymath.Clean.Cleaner
  ( cleanup
  , zeroDegree
  ) where

import Data.List(sortBy)

zeroDegree :: (Eq a, Num a) => [(a, a)]
zeroDegree = [(0, 0)]

--cleanup :: (Eq a, Num a) => [(a, a)] -> [a]
cleanup p = let sorted = sortBy sortDegree p
            in pad (foldl collectTerms zeroDegree sorted) []

collectTerms :: (Eq a, Num a) => [(a, a)] -> (a, a) -> [(a, a)]
collectTerms (x:xs) (c, d) = if d == snd x
                        then merge x (c, d) : xs
                        else (c, d):x:xs

sortDegree :: (Num a, Ord a) => (a, a) -> (a, a) -> Ordering
sortDegree (x, a) (y, b) = compare a b

merge :: Num a => (a, a) -> (a, a) -> (a, a)
merge (x1, y1) (x2, _) = (x1 + x2, y1)

pad :: (Num a, Ord a) => [(a, a)] -> [a] -> [a]
pad []   m = m
pad term m = let xs = init term
                 dg = snd $ last term
                 cf = fst $ last term
             in if dg > fromIntegral (length m)
                then pad term (0:m)
                else pad xs (cf:m)
