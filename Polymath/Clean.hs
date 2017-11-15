module Polymath.Clean
  ( toPoly
  ) where

import Data.Either(fromRight)
import Text.ParserCombinators.Parsec

import Polymath.Clean.Cleaner
import Polymath.Clean.Parser
import Polymath.Clean.Preprocessor

toPoly :: Monad m => String -> m [Integer]
toPoly str = do
  let s = sanitize str
  let mess = either (return $ cleanup mess) (return [])  zeroDegree (parse polynomial "fail" s)
  either (return []) (return cleanup) mess
