{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Time            (toGregorian)
import qualified Data.Vector          as V
import           Helper               (csvHeaders, samplePost)
import           Minecart
import           Minecart.Types
import           Test.Hspec

main :: IO ()
main = hspec $ do
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

  describe "command line parser" $ do
    it "returns correct option from command line arguments" $ do
      currentOption ["--pgsetup"]  `shouldBe` Right PgSetup
      currentOption ["--entities"] `shouldBe` Right CollectEntities

    it "ignores arguments after the first option" $
      currentOption ["--elastic", "--pgsetup"] `shouldBe` Right ElasticsearchIndex

    it "sends the instructions if no argument is given" $ do
      let (Left xs) = currentOption []
      xs `shouldContain` "Usage: "

    it "sends the instructions if an invalid argument is given" $ do
      let (Left xs) = currentOption ["--aksjdhakjd"]
      xs `shouldContain` "Usage: "

  describe "CSV parser" $
    it "should parse the sample file correctly" $ do
      Right (decodedHeaders, posts) <- decodeByName <$> BL.readFile "sample-data/gb-forum-sample.csv"
      decodedHeaders `shouldBe` csvHeaders
      V.head posts   `shouldBe` samplePost
