{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString, intercalate, toStrict)
import Data.Csv
import Data.Time
import Data.Vector
import Minecart
import Minecart.Types
import Test.Hspec

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
      currentOption ["--pgsetup"] `shouldBe` Right PgSetup
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
    it "should parse records correctly" $ do
      let allHeaders = [ "post_id"
                       , "thread_id"
                       , "user_id"
                       , "username"
                       , "subject"
                       , "body"
                       , "date"
                       , "visible"
                       , "moderated"
                       ]
          rawH = intercalate "," allHeaders `mappend` "\r\n"
          r    = "123,232,12,user1,hello,hello world,02-Jan-13,TRUE,TRUE\r\n"
          expectedPost = Post
                          123 232 12
                          "user1" "hello" "hello world"
                          (Just $ fromGregorian 2013 1 2)
                          True True
                          0 0
                          empty empty
          (Right (hd, res)) = decodeByName $ rawH `mappend` r
      hd  `shouldBe` fromList (toStrict <$> allHeaders)
      res `shouldBe` fromList [expectedPost]
