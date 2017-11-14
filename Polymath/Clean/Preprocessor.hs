module Polymath.Clean.Preprocessor
  ( sanitize
  ) where

import Data.Char(isLetter)

varChar :: Char
varChar = 'x'

allowedChars :: String
allowedChars = varChar : "1234567890^-+"

isAllowed :: Char -> Bool
isAllowed c = c `elem` allowedChars

changeToX :: Char -> Char
changeToX c = if isLetter c then varChar
              else c

sanitize :: String -> String
sanitize = filter isAllowed . map changeToX
