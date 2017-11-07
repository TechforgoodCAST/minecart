{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Minecart.Types where

import Data.Aeson       (FromJSON, ToJSON, object, toJSON, (.=))
import Data.Csv         hiding (Parser, (.=))
import Data.Text.Lazy
import Data.Time
import GHC.Generics     (Generic)
import Minecart.Parsers

type MessageId = Int

data Post =
  Post {
    userId                     :: Int
  , topicId                    :: Int
  , forumId                    :: Int
  , messageId                  :: MessageId
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

type EntityData = (MessageId, Entity)

data Entity =
  Entity {
    name                     :: String
  , salience                 :: Double
  , entitySentimentMagnitude :: Double
  , entitySentimentScore     :: Double
  } deriving (Eq, Show, Generic)

type SentimentScore = Double
type SentimentMagnitude = Double
type SentenceData = (MessageId, SentimentScore, SentimentMagnitude, Sentence)

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
  parseNamedRecord r = Post <$> r .: "UserID"
                            <*> r .: "TopicID"
                            <*> r .: "ForumID"
                            <*> r .: "MessageID"
                            <*> r .: "UserName"
                            <*> r .: "Subject"
                            <*> r .: "Body"
                            <*> (toDay <$> (r .: "CreationDate"))
                            <*> (toBool <$> (r .: "Visible"))
                            <*> (toBool <$> (r .: "Moderated"))
                            <*> return 0
                            <*> return 0
                            <*> return []
                            <*> return []

instance FromNamedRecord (Int, Entity) where
  parseNamedRecord r = do
    mId <- r .: "MessageId"
    n   <- r .: "EntityName"
    s   <- r .: "Salience"
    sm  <- r .: "SentimentMagnitude"
    ss  <- r .: "SentimentScore"
    return (mId, Entity n s sm ss)

instance FromNamedRecord (Int, Double, Double, Sentence) where
  parseNamedRecord r = do
    mId <- r .: "MessageId"
    dss <- r .: "DocumentSentimentScore"
    sdm <- r .: "DocumentSentimentMagnitude"
    s   <- r .: "Sentence"
    ss  <- r .: "SentenceSentimentScore"
    ssm <- r .: "SentenceSentimentMagnitude"
    return (mId, dss, sdm, Sentence s ss ssm)

data PostMapping = PostMapping

instance ToJSON PostMapping where
  toJSON PostMapping =
    object
      [ "properties" .=
          object [
            "userId"                     `ofType` "long"
          , "topidId"                    `ofType` "long"
          , "forumId"                    `ofType` "long"
          , "messageId"                  `ofType` "long"
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
