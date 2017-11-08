{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cloudcart.Types where

import           Control.Monad  (mzero)
import           Data.Aeson
import           Data.Csv       hiding ((.:), (.=))
import qualified Data.Csv       as C
import           Data.Text.Lazy (Text)

data GoogleConfig =
  GoogleConfig {
    apiKey      :: String
  , requestType :: GoogleRequestType
  , outputPath  :: String
  }

data GoogleRequestType =
    EntitySentimentAnalysis
  | SentimentAnalysis

newtype GoogleRequestText = GoogleRequestText Text

instance ToJSON GoogleRequestText where
  toJSON (GoogleRequestText text) =
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
  , entitySentimentScore     :: Double
  , entitySentimentMagnitude :: Double
  } deriving Show


data SentenceMeta = SentenceMeta Int Double Double Sentence

data Sentence =
  Sentence {
    sentence                   :: Text
  , sentenceSentimentScore     :: Double
  , sentenceSentimentMagnitude :: Double
  } deriving Show

data Sentences =
  Sentences {
    sentencePostId             :: Int
  , documentSentimentScore     :: Double
  , documentSentimentMagnitude :: Double
  , sentences                  :: [Sentence]
  } deriving Show

newtype Entities = Entities [Entity] deriving Show

instance FromJSON Entities where
  parseJSON (Object v) = Entities <$> v .: "entities"

instance FromJSON Entity where
  parseJSON (Object v) =
    Entity <$> return 0
           <*> v .: "name"
           <*> v .: "salience"
           <*> ((v .: "sentiment") >>= (.: "score"))
           <*> ((v .: "sentiment") >>= (.: "magnitude"))
  parseJSON _ = mzero

instance FromJSON Sentences where
  parseJSON (Object v) =
    Sentences <$> return 0
              <*> ((v .: "documentSentiment") >>= (.: "score"))
              <*> ((v .: "documentSentiment") >>= (.: "magnitude"))
              <*> v .: "sentences"

instance FromJSON Sentence where
  parseJSON (Object v) =
    Sentence <$> ((v .: "text") >>= (.: "content"))
             <*> ((v .: "sentiment") >>= (.: "score"))
             <*> ((v .: "sentiment") >>= (.: "magnitude"))

instance FromNamedRecord Post where
  parseNamedRecord r =
    Post <$> r C..: "post_id"
         <*> r C..: "body"

instance ToRecord Entity where
  toRecord (Entity pId n s esm ess) =
    record [ toField pId
           , toField n
           , toField s
           , toField esm
           , toField ess
           ]

instance ToRecord SentenceMeta where
  toRecord (SentenceMeta pId dss dsm (Sentence s sss ssm)) =
    record [ toField pId
           , toField dss
           , toField dsm
           , toField s
           , toField sss
           , toField ssm
           ]
