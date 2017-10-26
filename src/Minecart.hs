{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (object, (.=))
import qualified Data.Aeson             as A
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               hiding (Parser, (.=))
import qualified Data.Csv.Streaming     as S
import           Data.Text              (pack)
import           Data.Text.Lazy         (Text)
import           Data.Time
import qualified Data.Vector            as V
import           Database.V5.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client
import           System.Environment     (getArgs)
import           Text.Trifecta

data Post =
  Post {
    username  :: Text
  , subject   :: Text
  , body      :: Text
  , date      :: Maybe Day
  , visible   :: Bool
  , moderated :: Bool
  } deriving (Eq, Show, Generic)


instance A.ToJSON Post where
  toJSON = A.genericToJSON A.defaultOptions

instance A.FromJSON Post where
  parseJSON = A.genericParseJSON A.defaultOptions

instance FromNamedRecord Post where
  parseNamedRecord r = Post <$> r .: "UserName"
                            <*> r .: "Subject"
                            <*> r .: "Body"
                            <*> (toDay <$> (r .: "CreationDate"))
                            <*> (toBool <$> (r .: "Visible"))
                            <*> (toBool <$> (r .: "Moderated"))

data PostMapping = PostMapping

instance A.ToJSON PostMapping where
  toJSON PostMapping =
    object
      [ "properties" .=
          object [
            "username"  .= object ["type" .= ("keyword" :: Text)]
          , "subject"   .= object ["type" .= ("text" :: Text)]
          , "body"      .= object ["type" .= ("text" :: Text)]
          , "date"      .= object ["type" .= ("date" :: Text)]
          , "visible"   .= object ["type" .= ("visible" :: Text)]
          , "moderated" .= object ["type" .= ("moderated" :: Text)]
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


mkBulkStream :: S.Records Post -> V.Vector BulkOperation
mkBulkStream = snd . foldr incId (0, V.empty)
  where
    incId post (n, stream) = (n + 1, V.snoc stream $ mkBulkOperation n post)

mkBulkOperation :: Integer -> Post -> BulkOperation
mkBulkOperation n post = BulkIndex gbIndex postMapping (DocId . pack $ show n) $ toJSON post


main :: IO ()
main = do
  csvPath <- head <$> getArgs
  x       <- BL.readFile csvPath
  case S.decodeByName x of
    Left err      -> putStrLn err
    Right (_, xs) -> runBH' $ do
      liftIO $ putStrLn "resetting index"
      _ <- deleteIndex gbIndex
      _ <- createIndex indexSettings gbIndex
      _ <- putMapping gbIndex postMapping PostMapping
      liftIO $ putStrLn "posting to elasticsearch"
      _ <- bulk $ mkBulkStream xs
      _ <- refreshIndex gbIndex
      liftIO $ putStrLn "done"
