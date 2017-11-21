{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import Minecart.Types
import Test.Hspec

main :: IO ()
main = hspec $
  describe "decoding the gb csv" $
    it "parses date format correctly" $ do
      let (Just d1) = toDay "01-Jan-13"
          (Just d2) = toDay "12-Apr-17"
          (Just d3) = toDay "30-Jun-09"
          (Just d4) = toDay "31-Jan-99"
      toGregorian d1 `shouldBe` (2013, 1, 1)
      toGregorian d2 `shouldBe` (2017, 4, 12)
      toGregorian d3 `shouldBe` (2009, 6, 30)
      toGregorian d4 `shouldBe` (1999, 1, 31)
