module Polymath.Clean
  ( toPoly
  ) where

import Data.Either
import Text.ParserCombinators.Parsec

import Polymath.Clean.Cleaner
import Polymath.Clean.Parser
import Polymath.Clean.Preprocessor

toPoly :: Monad m => String -> m [Integer]
toPoly str = do
  let s = sanitize str
  let mess = parse polynomial "" s
  return $ either (\m -> []) cleanup mess
