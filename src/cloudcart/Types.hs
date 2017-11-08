{-# LANGUAGE OverloadedStrings #-}

module Cloudcart.Types where

import           Control.Monad  (mzero)
import           Data.Aeson
import           Data.Csv       hiding ((.:), (.=))
import qualified Data.Csv       as Csv
import           Data.Text.Lazy (Text)

newtype GoogleRequest = GoogleRequest Text

instance ToJSON GoogleRequest where
  toJSON (GoogleRequest text) =
    object [
      "encodingType" .= encoding
    , "document"     .= object [
          "type"     .= type'
        , "language" .= lan
        , "content"  .= text
        ]
    ]
    where type'    = "PLAIN_TEXT" :: Text
          lan      = "en"         :: Text
          encoding = "UTF8"       :: Text

data Post =
  Post {
    postId :: Int
  , body   :: Text
  } deriving Show

data Entity =
  Entity {
    entityPostId             :: Int
  , name                     :: String
  , salience                 :: Double
  , entitySentimentMagnitude :: Double
  , entitySentimentScore     :: Double
  } deriving Show

data Sentence =
  Sentence {
    sentencePostId             :: Int
  , documentSentimentScore     :: Double
  , documentSentimentMagnitude :: Double
  , sentence                   :: Text
  , sentenceSentimentScore     :: Double
  , sentenceSentimentMagnitude :: Double
  } deriving Show

newtype Entities = Entities [Entity] deriving Show

instance FromJSON Entities where
  parseJSON (Object v) = Entities <$> (v .: "entities")
  parseJSON _          = mzero

instance FromJSON Entity where
  parseJSON (Object v) = Entity <$> return 0
                                <*> v .: "name"
                                <*> v .: "salience"
                                <*> ((v .: "sentiment") >>= (.: "magnitude"))
                                <*> ((v .: "sentiment") >>= (.: "score"))
  parseJSON _ = mzero

instance FromNamedRecord Post where
  parseNamedRecord r = Post <$> r Csv..: "post_id"
                            <*> r Csv..: "body"

instance ToRecord Entity where
  toRecord (Entity pId n s esm ess) =
    record [ toField pId
           , toField n
           , toField s
           , toField esm
           , toField ess
           ]
