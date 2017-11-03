{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON, defaultOptions,
                                         genericParseJSON, genericToJSON,
                                         object, (.=))
import qualified Data.Aeson             as A
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               hiding (Parser, (.=))
import qualified Data.Csv.Streaming     as S
import qualified Data.IntMap            as IM
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (pack)
import           Data.Text.Lazy         (Text)
import           Data.Time              (Day, fromGregorian)
import qualified Data.Vector            as V
import           Database.V5.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings)
import           System.Environment     (getArgs)
import           Text.Trifecta          (Parser, Result (..), char, integer,
                                         parseString, string, try)
data Post =
  Post {
    userId    :: Int
  , topicId   :: Int
  , forumId   :: Int
  , messageId :: Int
  , username  :: Text
  , subject   :: Text
  , body      :: Text
  , date      :: Maybe Day
  , visible   :: Bool
  , moderated :: Bool
  , entities  :: [Entity]
  } deriving (Eq, Show, Generic)

data Entity =
  Entity {
    messageId'         :: Int
  , name               :: String
  , salience           :: Double
  , sentimentMagnitude :: Double
  , sentimentScore     :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Entity where
  toJSON = genericToJSON defaultOptions

instance ToJSON Post where
  toJSON = genericToJSON defaultOptions

instance FromJSON Entity where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Post where
  parseJSON = genericParseJSON defaultOptions

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
                            <*> return []

instance FromNamedRecord Entity where
  parseNamedRecord r = Entity <$> r .: "MessageId"
                              <*> r .: "EntityName"
                              <*> r .: "Salience"
                              <*> r .: "SentimentMagnitude"
                              <*> r .: "SentimentScore"

data PostMapping = PostMapping

instance A.ToJSON PostMapping where
  toJSON PostMapping =
    object
      [ "properties" .=
          object [
            "userId"    .= object ["type" .= ("integer" :: Text)]
          , "topidId"   .= object ["type" .= ("integer" :: Text)]
          , "forumId"   .= object ["type" .= ("integer" :: Text)]
          , "messageId" .= object ["type" .= ("integer" :: Text)]
          , "username"  .= object ["type" .= ("keyword" :: Text)]
          , "subject"   .= object ["type" .= ("text" :: Text)]
          , "body"      .= object ["type" .= ("text" :: Text)]
          , "date"      .= object ["type" .= ("date" :: Text)]
          , "visible"   .= object ["type" .= ("visible" :: Text)]
          , "moderated" .= object ["type" .= ("moderated" :: Text)]
          , "entities"  .= object ["type" .= ("nested" :: Text)]
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


mkBulkStream :: S.Records Entity -> S.Records Post -> V.Vector BulkOperation
mkBulkStream entities = snd . foldr incId (0, V.empty)
  where
    enMap = messageEntities entities
    incId post (n, stream) = (n + 1, V.snoc stream $ mkBulkOperation n enMap post)

mkBulkOperation :: Integer -> IM.IntMap [Entity] -> Post -> BulkOperation
mkBulkOperation n enMap post = BulkIndex gbIndex postMapping (DocId . pack $ show n) $ toJSON updatedPost
  where
    updatedPost = setEntities post . fromMaybe [] . IM.lookup (messageId post) $ enMap

messageEntities :: S.Records Entity -> IM.IntMap [Entity]
messageEntities = foldr accum IM.empty
  where accum a = IM.insertWith (++) (messageId' a) [a]

setEntities :: Post -> [Entity] -> Post
setEntities post xs = post { entities = xs }

handleElastic :: S.Records Entity -> S.Records Post -> BH IO ()
handleElastic entities posts = do
  liftIO $ putStrLn "resetting index"
  _ <- deleteIndex gbIndex
  _ <- createIndex indexSettings gbIndex
  _ <- putMapping gbIndex postMapping PostMapping
  liftIO $ putStrLn "posting to elasticsearch"
  _ <- bulk $ mkBulkStream entities posts
  _ <- refreshIndex gbIndex
  liftIO $ putStrLn "done"

main :: IO ()
main = do
  csvsPath <- head <$> getArgs
  x <- BL.readFile $ csvsPath <> "gb-forum.csv"
  y <- BL.readFile $ csvsPath <> "gb-forum-entities.csv"
  case S.decodeByName x of
    Left err         -> putStrLn err
    Right (_, posts) ->
      case S.decodeByName y of
        Left err            -> putStrLn err
        Right (_, entities) -> runBH' $ handleElastic entities posts
