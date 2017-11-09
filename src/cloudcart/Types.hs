{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Cloudcart.Types where

import Control.Monad  (mzero)
import Data.Aeson
import Data.Csv       hiding ((.:), (.=))
import Data.Text.Lazy (Text)
import GHC.Generics   (Generic)
import Prelude        hiding (lookup)

data GoogleConfig =
  GoogleConfig {
    apiKey      :: String
  , requestType :: GoogleRequestType
  , outputPath  :: String
  }

data GoogleRequestType =
    EntityAnalysis
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

type PostId = Int

data Post = Post { postId :: PostId, body :: Text }

data Entity =
  Entity {
    entityPostId             :: PostId
  , name                     :: String
  , salience                 :: Double
  , entitySentimentScore     :: Double
  , entitySentimentMagnitude :: Double
  } deriving (Show, Generic)

newtype Entities = Entities [Entity] deriving Show

data SentenceSentiment =
  SentenceSentiment {
    sentimentPostId            :: PostId
  , documentSentimentScore     :: Double
  , documentSentimentMagnitude :: Double
  , sentence                   :: Text
  , sentenceSentimentScore     :: Double
  , sentenceSentimentMagnitude :: Double
  } deriving (Show, Generic)

data RawSentimentData =
  RawSentimentData {
    sentimentPostId'            :: PostId
  , documentSentimentScore'     :: Double
  , documentSentimentMagnitude' :: Double
  , sentences'                  :: [RawSentence]
  }

data RawSentence =
  RawSentence {
    sentence'                   :: Text
  , sentenceSentimentScore'     :: Double
  , sentenceSentimentMagnitude' :: Double
  }

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

instance FromJSON RawSentimentData where
  parseJSON (Object v) =
    RawSentimentData <$> return 0
                     <*> ((v .: "documentSentiment") >>= (.: "score"))
                     <*> ((v .: "documentSentiment") >>= (.: "magnitude"))
                     <*> v .: "sentences"

instance FromJSON RawSentence where
  parseJSON (Object v) =
    RawSentence <$> ((v .: "text") >>= (.: "content"))
             <*> ((v .: "sentiment") >>= (.: "score"))
             <*> ((v .: "sentiment") >>= (.: "magnitude"))

instance FromNamedRecord Post where
  parseNamedRecord r =
    Post <$> r `lookup` "post_id"
         <*> r `lookup` "body"

instance DefaultOrdered Entity where
instance ToNamedRecord Entity where
  toNamedRecord (Entity pId n s ess esm) =
    namedRecord [ namedField "post_id" pId
                , namedField "name" n
                , namedField "salience" s
                , namedField "entity_sentiment_score" ess
                , namedField "entity_sentiment_mangnitude" esm
                ]

instance DefaultOrdered SentenceSentiment
instance ToNamedRecord SentenceSentiment where
  toNamedRecord (SentenceSentiment pId dss dsm s sss ssm) =
    namedRecord [ namedField "post_id" pId
                , namedField "document_sentiment_score" dss
                , namedField "document_sentiment_magnitude" dsm
                , namedField "sentence" s
                , namedField "sentence_sentiment_score" sss
                , namedField "sentence_sentiment_magnitude" ssm
                ]
