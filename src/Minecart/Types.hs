{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Minecart.Types where

import           Control.Monad                      (mzero)
import           Data.Aeson
import           Data.Csv                           hiding (toField, (.:), (.=))
import qualified Data.Csv                           as C
import           Data.Text.Lazy
import           Data.Time
import           Database.PostgreSQL.Simple         (Connection)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics                       (Generic)
import           Minecart.Parsers

-- Core Data Types

data Post =
  Post {
    postId                     :: Int
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

data Entity =
  Entity {
    entityPostId             :: Int
  , name                     :: Text
  , salience                 :: Double
  , entitySentimentScore     :: Double
  , entitySentimentMagnitude :: Double
  } deriving (Eq, Show, Generic)

data Sentence =
  Sentence {
    sentencePostId             :: Int
  , sentence                   :: Text
  , sentenceSentimentScore     :: Double
  , sentenceSentimentMagnitude :: Double
  } deriving (Eq, Show, Generic)


newtype Entities = Entities [Entity] deriving Show
data Sentiment = Sentiment Double Double [Sentence] deriving Show
newtype GoogleRequest = GoogleRequest Text deriving Show

data GoogleConfig =
  GoogleConfig {
    apiKey      :: String
  , requestType :: String
  , dbConn      :: Connection
  }

data PostMapping = PostMapping


-- CSV

instance FromNamedRecord Post where
  parseNamedRecord r =
    Post <$> r `C.lookup` "post_id"
         <*> r `C.lookup` "thread_id"
         <*> r `C.lookup` "user_id"
         <*> r `C.lookup` "username"
         <*> r `C.lookup` "subject"
         <*> r `C.lookup` "body"
         <*> (toDay <$> (r `C.lookup` "date"))
         <*> (toBool <$> (r `C.lookup` "visible"))
         <*> (toBool <$> (r `C.lookup` "moderated"))
         <*> return 0
         <*> return 0
         <*> return []
         <*> return []


-- Database

instance FromRow Post where
  fromRow = Post <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> return []
                 <*> return []

instance ToRow Post where
  toRow p =
    [ toField $ postId p
    , toField $ threadId p
    , toField $ userId p
    , toField $ username p
    , toField $ subject p
    , toField $ body p
    , toField $ date p
    , toField $ visible p
    , toField $ moderated p
    , toField $ documentSentimentScore p
    , toField $ documentSentimentMagnitude p
    ]

instance FromRow Entity where
  fromRow = do
    _ <- field :: RowParser Int
    Entity <$> field
           <*> field
           <*> field
           <*> field
           <*> field

instance ToRow Entity where
  toRow e =
    [ toField $ entityPostId e
    , toField $ name e
    , toField $ salience e
    , toField $ entitySentimentScore e
    , toField $ entitySentimentMagnitude e
    ]

instance FromRow Sentence where
  fromRow = do
    _ <- field :: RowParser Int
    Sentence <$> field
             <*> field
             <*> field
             <*> field

instance ToRow Sentence where
  toRow s =
    [ toField $ sentencePostId s
    , toField $ sentence s
    , toField $ sentenceSentimentScore s
    , toField $ sentenceSentimentMagnitude s
    ]


-- JSON

instance ToJSON Post

instance FromJSON Entities where
  parseJSON (Object v) = Entities <$> v .: "entities"
  parseJSON _          = mzero

instance ToJSON Entity
instance FromJSON Entity where
  parseJSON (Object v) =
    Entity <$> return 0
           <*> v .: "name"
           <*> v .: "salience"
           <*> ((v .: "sentiment") >>= (.: "score"))
           <*> ((v .: "sentiment") >>= (.: "magnitude"))
  parseJSON _ = mzero

instance FromJSON Sentiment where
  parseJSON (Object v) =
    Sentiment <$> ((v .: "documentSentiment") >>= (.: "score"))
              <*> ((v .: "documentSentiment") >>= (.: "magnitude"))
              <*> v .: "sentences"
  parseJSON _ = mzero

instance ToJSON Sentence
instance FromJSON Sentence where
  parseJSON (Object v) =
    Sentence <$> return 0
             <*> ((v .: "text") >>= (.: "content"))
             <*> ((v .: "sentiment") >>= (.: "score"))
             <*> ((v .: "sentiment") >>= (.: "magnitude"))
  parseJSON _ = mzero

instance ToJSON GoogleRequest where
  toJSON (GoogleRequest body) =
    object [
      "encodingType" .= encoding
    , "document"     .= object [
          "type"     .= type'
        , "language" .= lan
        , "content"  .= body
        ]
    ]
    where type'    = "PLAIN_TEXT" :: Text
          lan      = "en"         :: Text
          encoding = "UTF8"       :: Text

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
