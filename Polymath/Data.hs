{-# LANGUAGE DeriveGeneric #-}

module Polymath.Data
  ( Operation(..)
  , OperationData(..)
  , Response(..)
  , opDescarte
  , opZeros
  , opSynthetic
  , ratioToInt
  , ratioToFrac
  ) where

import Data.Aeson
import GHC.Generics
import Data.Ratio

data Response = Response { steps :: [Operation]
                         , roots :: [Fraction]
                         } deriving (Generic, Show)

data Operation = Operation { opType :: String
                           , title :: String
                           , dat :: OperationData
                           } deriving (Generic, Show)

data OperationData = Descarte { max_positive :: Integer
                              , max_negative :: Integer
                              }
                   | RationalZeros { potential_zeros :: [Fraction]
                                   , actual_zeros :: [Fraction]
                                   }
                   | SyntheticDiv { top :: [Fraction]
                                  --, middle :: [Fraction]
                                  , bottom :: [Fraction]
                                  , input :: [Integer]
                                  , polynomial :: [Fraction]
                                  , remainder :: Fraction
                                  , root :: Float
                                  }
                   deriving (Generic, Show)

data Fraction = Fraction { num :: Integer
                         , denom :: Integer
                         } deriving (Generic, Show)

ratioToFrac :: Integral a => Ratio a -> Fraction
ratioToFrac r = Fraction { num = toInteger $ numerator r
                         , denom = toInteger $ denominator r
                         }

ratioToInt :: Integral a => Ratio a -> a
ratioToInt r = numerator r `div` denominator r

intToFrac :: Integral a => a -> Fraction
intToFrac i = Fraction { num = toInteger i
                       , denom = 1 :: Integer
                       }

opDescarte :: Integral a => a -> a -> Operation
opDescarte p n =
  let d = Descarte { max_positive = toInteger p
                   , max_negative = toInteger n
                   }
  in Operation { opType = "descarte"
               , title = "Rule of Signs"
               , dat = d
               }

opZeros :: Integral a => [Ratio a] -> [Ratio a] -> Operation
opZeros p a =
  let d = RationalZeros { potential_zeros = map ratioToFrac p
                        , actual_zeros = map ratioToFrac a
                        }
  in Operation { opType = "zeros"
               , title = "Rational Zeros Test"
               , dat = d
               }

opSynthetic :: Integral a => [a] -> [Ratio a] -> Operation
opSynthetic i s =
  let d = SyntheticDiv { top = map intToFrac i
                       --, middle = map ratioToFrac m
                       , bottom = map ratioToFrac s
                       , input = map toInteger i
                       , polynomial = map ratioToFrac s
                       , remainder = ratioToFrac $ last s
                       , root = 0.0
                       }
  in Operation { opType = "synthetic_division"
               , title = "Synthetic Division"
               , dat = d
               }


instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Operation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON OperationData where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Fraction where
  toEncoding = genericToEncoding defaultOptions
