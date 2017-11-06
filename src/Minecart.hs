{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative        ((<|>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson                 (FromJSON, ToJSON, defaultOptions,
                                             genericParseJSON, genericToJSON,
                                             object, (.=))
import qualified Data.Aeson                 as A
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.Csv                   hiding (Parser, (.=))
import qualified Data.Csv.Streaming         as S
import qualified Data.IntMap                as IM
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (pack)
import           Data.Text.Lazy             (Text)
import           Data.Time                  (Day, fromGregorian)
import qualified Data.Vector                as V
import           Database.V5.Bloodhound
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (defaultManagerSettings)
import           System.Environment         (getArgs)
import           Text.Trifecta              (Parser, Result (..), char, integer,
                                             parseString, string, try)

data Post =
  Post {
    userId                     :: Int
  , topicId                    :: Int
  , forumId                    :: Int
  , messageId                  :: Int
  , username                   :: Text
  , subject                    :: Text
  , body                       :: Text
  , date                       :: Maybe Day
  , visible                    :: Bool
  , moderated                  :: Bool
  , documentSentimentScore     :: Double
  , documentSentimentMagnitude :: Double
  , entities                   :: [Entity]
  } deriving (Eq, Show, Generic)

data Entity =
  Entity {
    name                     :: String
  , salience                 :: Double
  , entitySentimentMagnitude :: Double
  , entitySentimentScore     :: Double
  } deriving (Eq, Show, Generic)

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

instance FromNamedRecord (Int, Entity) where
  -- parseNamedRecord r = Entity <$> r .: "MessageId"
  --                             <*> r .: "EntityName"
  --                             <*> r .: "Salience"
  --                             <*> r .: "SentimentMagnitude"
  --                             <*> r .: "SentimentScore"
  parseNamedRecord r = do
    mId <- r .: "MessageId"
    n   <- r .: "EntityName"
    s   <- r .: "Salience"
    sm  <- r .: "SentimentMagnitude"
    ss  <- r .: "SentimentScore"
    return (mId, Entity n s sm ss)

instance FromNamedRecord (Int, Double, Double, Sentence) where
  -- 'MessageId',
  -- 'DocumentSentimentScore',
  -- 'DocumentSentimentMagnitude',
  -- 'Sentence',
  -- 'SentenceSentimentScore',
  -- 'SentenceSentimentMagnitude'
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
    let
      ofType k x = k .= object ["type" .= (x :: Text)]
    in
    object
      [ "properties" .=
          object [
            "userId"    `ofType` "integer"
          , "topidId"   `ofType` "integer"
          , "forumId"   `ofType` "integer"
          , "messageId" `ofType` "integer"
          , "username"  `ofType` "keyword"
          , "subject"   `ofType` "text"
          , "body"      `ofType` "text"
          , "date"      `ofType` "date"
          , "visible"   `ofType` "visible"
          , "moderated" `ofType` "moderated"
          , "entities"  `ofType` "nested"
          ]
      ]

toBool :: ByteString -> Bool
toBool "TRUE"  = True
toBool "FALSE" = False
toBool _       = False

toDay :: ByteString -> Maybe Day
toDay rawDate =
  case parseString dateParser mempty $ show rawDate of
    Success d -> Just d
    Failure _ -> Nothing

dateParser :: Parser Day
dateParser = do
  _ <- char '\"'
  d <- integer
  _ <- char '-'
  m <- parseMonth
  _ <- char '-'
  y <- integer
  return $ fromGregorian (2000 + y) m (fromIntegral d)

parseMonth :: Parser Int
parseMonth = month 1  "Jan" <|>
             month 2  "Feb" <|>
             month 3  "Mar" <|>
             month 4  "Apr" <|>
             month 5  "May" <|>
             month 6  "Jun" <|>
             month 7  "Jul" <|>
             month 8  "Aug" <|>
             month 9  "Sep" <|>
             month 10 "Oct" <|>
             month 11 "Nov" <|>
             month 12 "Dec"
  where month n m = try $ const n <$> string m

runBH'        = withBH defaultManagerSettings server
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
gbIndex       = IndexName "gingerbread"
postMapping   = MappingName "post"
server        = Server "http://localhost:9200"

mkBulkStream :: S.Records (Int, Entity) -> S.Records Post -> V.Vector BulkOperation
mkBulkStream entities = snd . foldr incId (0, V.empty)
  where
    enMap = messageEntities entities
    incId post (n, stream) = (n + 1, V.snoc stream $ mkBulkOperation n enMap post)

mkBulkOperation :: Integer -> IM.IntMap [Entity] -> Post -> BulkOperation
mkBulkOperation n enMap post = BulkIndex gbIndex postMapping (DocId . pack $ show n) $ toJSON updatedPost
  where
    updatedPost = setEntities post . fromMaybe [] . IM.lookup (messageId post) $ enMap

messageEntities :: S.Records (Int, Entity) -> IM.IntMap [Entity]
messageEntities = foldr accum IM.empty
  where accum (mId, a) = IM.insertWith (++) mId [a]

setEntities :: Post -> [Entity] -> Post
setEntities post xs = post { entities = xs }

handleElastic :: S.Records (Int, Entity) -> S.Records Post -> BH IO ()
handleElastic entities posts = do
  deleteIndex gbIndex
  createIndex indexSettings gbIndex
  putMapping gbIndex postMapping PostMapping

  liftIO $ putStrLn "posting data to elasticsearch"
  bulk $ mkBulkStream entities posts
  refreshIndex gbIndex
  return ()

main :: IO ()
main = do
  csvsPath <- head <$> getArgs
  x <- BL.readFile $ csvsPath <> "gb-forum.csv"
  y <- BL.readFile $ csvsPath <> "gb-forum-entities.csv"
  runExceptT $ do
    (_, posts)    <- lift $ S.decodeByName x
    (_, entities) <- lift $ S.decodeByName y
    liftIO . runBH' $ handleElastic entities posts
  putStrLn "done"
  where lift = ExceptT . return
