module Main where

import Polymath.Factor
import Polymath.Clean
import Data.Ratio

man :: IO ()
man = hspec $ do

  describe "verify polynomial cleaner" $
    it "should cleanup the polynomial into a list of ints" $
      toPoly "12x^3-41x^2-38x+40" `shouldBe` [12, -41, -38, 40]

  describe "verify polynomial factoring" $
    it "should give 3 real roots for the previous polynomial" $
      factor [12, -41, -38, 40] `shouldBe` [4 % 1, 2 % 3, -5 % 4]
