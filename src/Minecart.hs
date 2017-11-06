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

runBH'        = withBH defaultManagerSettings server
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
gbIndex       = IndexName "gingerbread"
postMapping   = MappingName "post"
server        = Server "http://localhost:9200"

mkBulkStream :: S.Records EntityData
             -> S.Records SentenceData
             -> S.Records Post
             -> V.Vector BulkOperation
mkBulkStream entities sentences = snd . foldr incId (0, V.empty)
  where
    enMap = messageEntities entities
    sMap  = messageSentences sentences
    incId post (n, stream) = (n + 1, V.snoc stream $ mkBulkOperation n enMap sMap post)

mkBulkOperation :: MessageId
                -> IM.IntMap [Entity]
                -> IM.IntMap [(SentimentScore, SentimentMagnitude, Sentence)]
                -> Post
                -> BulkOperation
mkBulkOperation n enMap sMap post = BulkIndex gbIndex postMapping (DocId . pack $ show n) $ toJSON updatedPost
  where
    entities    = IM.lookup (messageId post) enMap
    sentences   = IM.lookup (messageId post) sMap
    sentiment   = getSentiment <$> sentences
    sentences'  = map (\(a, b, c) -> c) <$> sentences
    updatedPost = setSentiment (fromMaybe (0, 0) sentiment) . setSentences (fromMaybe [] sentences') . setEntities (toList entities) $ post
    toList      = fromMaybe []

messageEntities :: S.Records EntityData -> IM.IntMap [Entity]
messageEntities = foldr accum IM.empty
  where
    accum (mId, a) = IM.insertWith (++) mId [a]

setEntities :: [Entity] -> Post -> Post
setEntities xs post = post { entities = xs }

getSentiment :: [(SentimentScore, SentimentMagnitude, Sentence)] -> (SentimentScore, SentimentMagnitude)
getSentiment [] = (0, 0)
getSentiment xs = (\(dss, dsm, _) -> (dss, dsm)) . head $ xs

messageSentences :: S.Records SentenceData -> IM.IntMap [(SentimentScore, SentimentMagnitude, Sentence)]
messageSentences = foldr accum IM.empty
  where
    accum (mId, dss, sdm, a) = IM.insertWith (++) mId [(dss, sdm, a)]

setSentences :: [Sentence] -> Post -> Post
setSentences xs post = post { sentences = xs }

setSentiment :: (SentimentScore, SentimentMagnitude) -> Post -> Post
setSentiment (dss, dsm) post = post { documentSentimentScore = dss, documentSentimentMagnitude = dsm }

handleElastic :: S.Records EntityData -> S.Records SentenceData -> S.Records Post -> BH IO ()
handleElastic entities sentences posts = do
  deleteIndex gbIndex
  createIndex indexSettings gbIndex
  putMapping gbIndex postMapping PostMapping

  liftIO $ putStrLn "posting data to elasticsearch"
  bulk $ mkBulkStream entities sentences posts
  refreshIndex gbIndex
  return ()

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
