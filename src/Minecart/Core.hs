{-# LANGUAGE OverloadedStrings #-}

module Minecart.Core
  ( setup
  , collectEntities
  , collectSentiments
  , postToElasticsearch
  , resetIndex
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy       as BL
import           Data.Csv                   (Header)
import qualified Data.Csv.Streaming         as S
import           Data.IntMap                (IntMap, empty, insertWith)
import qualified Data.IntMap                as IM
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (pack)
import           Data.Vector                (Vector, fromList)
import qualified Database.PostgreSQL.Simple as PG
import           Database.V5.Bloodhound
import qualified Minecart.Database          as DB
import qualified Minecart.GoogleCloud       as Cloud
import           Minecart.Types
import           Network.HTTP.Client
import           System.Environment         (getEnv)

-- Setup

setup :: IO ()
setup = do
  conn  <- DB.minecartConn
  decodeForum >>= either print (setupDB conn)

setupDB :: PG.Connection -> (Header, S.Records Post) -> IO ()
setupDB conn (_, posts) = do
  putStrLn "creating DB tables"
  DB.createTables conn
  putStrLn "inserting all posts"
  DB.insertAllPosts conn posts
  putStrLn "setup complete"

decodeForum :: IO (Either String (Header, S.Records Post))
decodeForum = S.decodeByName <$> BL.readFile "../gb-forum.csv"


-- Collect GoogleCloud Data

collectEntities :: IO ()
collectEntities = do
  k     <- loadApiKey
  conn  <- DB.minecartConn
  posts <- DB.remainingEntityPosts conn
  let config = GoogleConfig k "analyzeEntitySentiment" conn
  Cloud.run config posts

collectSentiments :: IO ()
collectSentiments = do
  k     <- loadApiKey
  conn  <- DB.minecartConn
  posts <- DB.remainingSentimentPosts conn
  let config = GoogleConfig k "analyzeSentiment" conn
  Cloud.run config posts

loadApiKey :: IO String
loadApiKey = getEnv "GOOGLE_CLOUD_API_KEY"


-- Elasticsearch

postToElasticsearch :: IO ()
postToElasticsearch = do
  stream <- loadBulkStream
  runBH' $ postBulk stream

postBulk :: Vector BulkOperation -> BH IO ()
postBulk xs = do
  liftIO $ putStrLn "posting to elasticsearch"
  bulk xs
  refreshIndex gbIndex
  liftIO $ putStrLn "posted to elasticsearch"

resetIndex :: IO ()
resetIndex = runBH' $ do
  deleteIndex gbIndex
  createIndex settings gbIndex
  putMapping gbIndex postMapping PostMapping
  liftIO $ putStrLn "reset index"
  where
    settings = IndexSettings (ShardCount 1) (ReplicaCount 0)

loadBulkStream :: IO (Vector BulkOperation)
loadBulkStream = do
  putStrLn "preparing bulk stream"
  conn <- DB.minecartConn
  ex   <- DB.allEntities conn
  sx   <- DB.allSentences conn
  px   <- DB.allPosts conn
  putStrLn "loaded bulk stream"
  return $ bulkStream ex sx px

bulkStream :: IntMap (Vector Entity) -> IntMap (Vector Sentence) -> Vector Post -> Vector BulkOperation
bulkStream ex sx = fmap bulkOperation . combineAll ex sx

bulkOperation :: Post -> BulkOperation
bulkOperation p = BulkIndex gbIndex postMapping (docId p) $ toJSON p
  where docId = DocId . pack . show . postId

runBH' :: BH IO a -> IO a
runBH' = withBH defaultManagerSettings server
  where server = Server "http://localhost:9200"

gbIndex :: IndexName
gbIndex = IndexName "gingerbread"

postMapping :: MappingName
postMapping = MappingName "post"


-- Combine All Data

combineAll :: IntMap (Vector Entity) -> IntMap (Vector Sentence) -> Vector Post -> Vector Post
combineAll em = fmap . combinedPost em

combinedPost :: IntMap (Vector Entity) -> IntMap (Vector Sentence) -> Post -> Post
combinedPost em sm p = fromMaybe p $ do
  let pId = postId p
  e <- IM.lookup pId em
  s <- IM.lookup pId sm
  return $ p { entities  = e
             , sentences = s
             }
