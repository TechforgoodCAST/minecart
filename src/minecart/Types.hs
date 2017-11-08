{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Minecart.Types where

import Data.Aeson       (FromJSON, ToJSON, object, toJSON, (.=))
import Data.Csv         hiding ((.=))
import Data.Text.Lazy
import Data.Time
import GHC.Generics     (Generic)
import Minecart.Parsers

type PostId = Int

data Post =
  Post {
    postId                     :: PostId
  , threadId                   :: Int
  , userId                     :: Int
  , username                   :: Text
  , subject                    :: Text
  , body                       :: Text
  , date                       :: Maybe Day
  , visible                    :: Bool
  , moderated                  :: Bool
  , documentSentimentScore     :: Double
  , documentSentimentMagnitude :: Double
  , entities                   :: [Entity]
  , sentences                  :: [Sentence]
  } deriving (Eq, Show, Generic)

type EntityData = (PostId, Entity)

data Entity =
  Entity {
    name                     :: String
  , salience                 :: Double
  , entitySentimentMagnitude :: Double
  , entitySentimentScore     :: Double
  } deriving (Eq, Show, Generic)

type SentimentScore = Double
type SentimentMagnitude = Double
type SentenceData = (PostId, SentimentScore, SentimentMagnitude, Sentence)

data Sentence =
  Sentence {
    sentence                   :: Text
  , sentenceSentimentScore     :: Double
  , sentenceSentimentMagnitude :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Post
instance FromJSON Post

instance ToJSON Entity
instance FromJSON Entity

instance ToJSON Sentence
instance FromJSON Sentence

instance FromNamedRecord Post where
  parseNamedRecord r =
    Post <$> r .: "post_id"
         <*> r .: "thread_id"
         <*> r .: "user_id"
         <*> r .: "username"
         <*> r .: "subject"
         <*> r .: "body"
         <*> (toDay <$> (r .: "date"))
         <*> (toBool <$> (r .: "visible"))
         <*> (toBool <$> (r .: "moderated"))
         <*> return 0
         <*> return 0
         <*> return []
         <*> return []

instance FromNamedRecord (Int, Entity) where
  parseNamedRecord r = do
    mId <- r .: "post_id"
    n   <- r .: "entity_name"
    s   <- r .: "salience"
    sm  <- r .: "sentiment_magnitude"
    ss  <- r .: "sentiment_score"
    return (mId, Entity n s sm ss)

instance FromNamedRecord (Int, Double, Double, Sentence) where
  parseNamedRecord r = do
    mId <- r .: "post_id"
    dss <- r .: "document_sentiment_score"
    sdm <- r .: "document_sentiment_magnitude"
    s   <- r .: "sentence"
    ss  <- r .: "sentence_sentiment_score"
    ssm <- r .: "sentence_sentiment_magnitude"
    return (mId, dss, sdm, Sentence s ss ssm)

data PostMapping = PostMapping

instance ToJSON PostMapping where
  toJSON PostMapping =
    object
      [ "properties" .=
          object [
            "postId"                     `ofType` "integer"
          , "userId"                     `ofType` "integer"
          , "threadId"                   `ofType` "integer"
          , "username"                   `ofType` "keyword"
          , "subject"                    `ofType` "text"
          , "body"                       `ofType` "text"
          , "date"                       `ofType` "date"
          , "visible"                    `ofType` "visible"
          , "moderated"                  `ofType` "moderated"
          , "documentSentimentScore"     `ofType` "long"
          , "documentSentimentMagnitude" `ofType` "long"
          , "entities"                   `ofType` "nested"
          , "sentences"                  `ofType` "nested"
          ]
      ]
    where ofType k x = k .= object ["type" .= (x :: Text)]
