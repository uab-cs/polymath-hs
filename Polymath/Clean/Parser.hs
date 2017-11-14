module Polymath.Clean.Parser
  ( polynomial
  ) where

import Text.ParserCombinators.Parsec

negative :: GenParser Char st Integer
negative = char '-' >> return (-1 :: Integer)

positive :: GenParser Char st Integer
positive = char '+' >> return (1 :: Integer)

sign :: GenParser Char st Integer
sign = try negative <|> try positive <|> return 1

num :: GenParser Char st Integer
num = sign >>= (\s -> many1 digit >>= (\val -> return $ s * read val))

expo :: GenParser Char st Integer
expo = char '^' >> num

varExpo :: GenParser Char st Integer
varExpo = char 'x' >> (try expo <|> return 1)

monomial :: GenParser Char st (Integer, Integer)
monomial = do
  coeff <- num -- <|> return 1
  deg <- try varExpo <|> return 0
  return (coeff, deg)

polynomial :: GenParser Char st [(Integer, Integer)]
polynomial = many1 monomial
