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
  , entitySentimentScore     :: Double
  , entitySentimentMagnitude :: Double
  } deriving Show

data Sentence =
  Sentence {
    sentencePostId             :: Int
  , sentence                   :: Text
  , documentSentimentScore     :: Double
  , documentSentimentMagnitude :: Double
  , sentenceSentimentScore     :: Double
  , sentenceSentimentMagnitude :: Double
  } deriving Show

instance FromJSON Entity where
  parseJSON (Object v) =
    let ePath = v .: "entities"
    in
    Entity <$> return 0
           <*> (ePath >>= (.: "name"))
           <*> (ePath >>= (.: "salience"))
           <*> (ePath >>= (.: "sentiment") >>= (.: "score"))
           <*> (ePath >>= (.: "sentiment") >>= (.: "magnitude"))
  parseJSON _ = mzero


instance FromJSON Sentence where
  parseJSON (Object v) =
    let sPath = v .: "sentences"
        dPath = v .: "documentSentiment"
    in
    Sentence <$> return 0
             <*> (sPath >>= (.: "text") >>= (.: "content"))
             <*> (dPath >>= (.: "score"))
             <*> (dPath >>= (.: "magnitude"))
             <*> (sPath >>= (.: "sentiment") >>= (.: "score"))
             <*> (sPath >>= (.: "sentiment") >>= (.: "magnitude"))


instance FromNamedRecord Post where
  parseNamedRecord r =
    Post <$> r Csv..: "post_id"
         <*> r Csv..: "body"

instance ToRecord Entity where
  toRecord (Entity pId n s esm ess) =
    record [ toField pId
           , toField n
           , toField s
           , toField esm
           , toField ess
           ]
