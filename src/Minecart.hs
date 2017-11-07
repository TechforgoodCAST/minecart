{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Csv.Streaming         as S
import qualified Data.IntMap                as IM
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (pack)
import           Data.Text.Lazy             (Text)
import qualified Data.Vector                as V
import           Database.V5.Bloodhound
import           Minecart.Types
import           Network.HTTP.Client        (defaultManagerSettings)
import           System.Environment         (getArgs)

-- Combine CSV data

mkBulkStream :: S.Records EntityData -> S.Records SentenceData -> S.Records Post -> V.Vector BulkOperation
mkBulkStream entities sentences = snd . foldr incId (0, V.empty)
  where
    incId post (n, stream) = (n + 1, V.snoc stream $ mkBulkOperation n enMap sMap post)
    enMap = entitiesMap entities
    sMap  = sentencesMap sentences

mkBulkOperation :: PostId -> IM.IntMap [Entity] -> IM.IntMap [(SentimentScore, SentimentMagnitude, Sentence)] -> Post -> BulkOperation
mkBulkOperation n enMap sMap post = BulkIndex gbIndex postMapping (DocId . pack $ show n) $ toJSON updatedPost
  where
    updatedPost = updatePost n enMap sMap post

updatePost :: PostId -> IM.IntMap [Entity] -> IM.IntMap [(SentimentScore, SentimentMagnitude, Sentence)] -> Post -> Post
updatePost n enMap sMap post = fromMaybe post $ do
  let pId = postId post
  es <- IM.lookup pId enMap
  ss <- IM.lookup pId sMap
  let ss' = thrd <$> ss
      sn  = getSentiment ss
      thrd (a, b, c) = c
  return . setSentiment sn . setSentences ss' . setEntities es $ post

entitiesMap :: S.Records EntityData -> IM.IntMap [Entity]
entitiesMap = foldr accum IM.empty
  where
    accum (pId, a) = IM.insertWith (++) pId [a]

sentencesMap :: S.Records SentenceData -> IM.IntMap [(SentimentScore, SentimentMagnitude, Sentence)]
sentencesMap = foldr accum IM.empty
  where
    accum (pId, ss, sm, a) = IM.insertWith (++) pId [(ss, sm, a)]

-- Lenses

setEntities :: [Entity] -> Post -> Post
setEntities xs post = post { entities = xs }

getSentiment :: [(SentimentScore, SentimentMagnitude, Sentence)] -> (SentimentScore, SentimentMagnitude)
getSentiment [] = (0, 0)
getSentiment xs = (\(ss, sm, _) -> (ss, sm)) . head $ xs

setSentences :: [Sentence] -> Post -> Post
setSentences xs post = post { sentences = xs }

setSentiment :: (SentimentScore, SentimentMagnitude) -> Post -> Post
setSentiment (ss, sm) post = post
  { documentSentimentScore = ss
  , documentSentimentMagnitude = sm
  }

-- Elasticsearch

handleElastic :: S.Records EntityData -> S.Records SentenceData -> S.Records Post -> BH IO ()
handleElastic entities sentences posts = do
  resetIndex
  liftIO $ putStrLn "posting data to elasticsearch"
  bulk $ mkBulkStream entities sentences posts
  refreshIndex gbIndex
  return ()

resetIndex :: BH IO ()
resetIndex = do
  liftIO $ putStrLn "resetting index"
  deleteIndex gbIndex
  createIndex indexSettings gbIndex
  putMapping gbIndex postMapping PostMapping
  return ()

runBH'        = withBH defaultManagerSettings server
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
gbIndex       = IndexName "gingerbread"
postMapping   = MappingName "post"
server        = Server "http://localhost:9200"

main :: IO ()
main = do
  csvsPath <- head <$> getArgs
  x <- BL.readFile $ csvsPath <> "gb-forum.csv"
  y <- BL.readFile $ csvsPath <> "gb-forum-entities.csv"
  z <- BL.readFile $ csvsPath <> "gb-forum-sentiments.csv"
  runExceptT $ do
    (_, posts)     <- lift $ S.decodeByName x
    (_, entities)  <- lift $ S.decodeByName y
    (_, sentences) <- lift $ S.decodeByName z
    liftIO . runBH' $ handleElastic entities sentences posts
  putStrLn "done"
  where lift = ExceptT . return
