{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Minecart.Database where

import           Data.Foldable              (toList)
import           Data.Int                   (Int64)
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM
import           Data.Monoid                ((<>))
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple
import           Minecart.Types
import           Text.RawString.QQ

-- Queries

createTablesQuery :: Query
createTablesQuery = [r|
CREATE TABLE posts (
  post_id integer PRIMARY KEY,
	thread_id integer,
	user_id integer,
	username text,
	subject text,
	body text,
	date date,
	visible boolean,
	moderated boolean,
  document_sentiment_score float,
  document_sentiment_magnitude float
);
CREATE TABLE entities (
  entity_id SERIAL PRIMARY KEY,
  post_id integer references posts(post_id),
  name text,
  salience float,
  sentiment_score float,
  sentiment_magnitude float
);
CREATE TABLE sentence_sentiments (
  sentence_sentiment_id SERIAL PRIMARY KEY,
  post_id integer references posts(post_id),
  sentence text,
  sentiment_score float,
  sentiment_magnitude float
);
CREATE TABLE cloud_response_statuses (
	post_id integer PRIMARY KEY,
	entities_queried boolean DEFAULT FALSE,
	sentiment_queried boolean DEFAULT FALSE
);|]

insertPostQuery :: Query
insertPostQuery = "INSERT INTO posts VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertEntityQuery :: Query
insertEntityQuery = [r|
INSERT INTO entities (post_id, name, salience, sentiment_score, sentiment_magnitude)
  VALUES (?, ?, ?, ?, ?)
|]

insertSentenceQuery :: Query
insertSentenceQuery = [r|
INSERT INTO sentence_sentiments (post_id, sentence, sentiment_score, sentiment_magnitude)
  VALUES (?, ?, ?, ?)
|]

updateDocumentSentimentQuery :: Query
updateDocumentSentimentQuery = [r|
UPDATE posts SET
  document_sentiment_score = ?,
  document_sentiment_magnitude = ?
  WHERE post_id = ?
|]

createStatusesQuery :: Query
createStatusesQuery = "INSERT INTO cloud_response_statuses (post_id) VALUES (?)"

updateEntitiesStatusQuery :: Query
updateEntitiesStatusQuery = [r|
UPDATE cloud_response_statuses SET entities_queried = TRUE
  WHERE post_id = ?
|]

updateSentimentStatusQuery :: Query
updateSentimentStatusQuery = [r|
UPDATE cloud_response_statuses SET sentiment_queried = TRUE
  WHERE post_id = ?
|]

remainingEntityPostsQuery :: Query
remainingEntityPostsQuery = [r|
SELECT * FROM posts P
WHERE NOT EXISTS (
  SELECT post_id, entities_queried FROM cloud_response_statuses S
  WHERE S.post_id = P.post_id
  AND s.entities_queried = TRUE
)|]

remainingSentimentPostsQuery :: Query
remainingSentimentPostsQuery = [r|
SELECT * FROM posts P
WHERE NOT EXISTS (
  SELECT post_id, sentiment_queried FROM cloud_response_statuses S
  WHERE S.post_id = P.post_id
  AND s.sentiment_queried = TRUE
)|]


-- DB Functions

createTables :: Connection -> IO Int64
createTables conn = execute_ conn createTablesQuery

allPosts :: Connection -> IO (Vector Post)
allPosts conn = fold_ conn "SELECT * FROM posts" V.empty f
  where
    f a = return . flip V.cons a

allEntities :: Connection -> IO (IntMap (Vector Entity))
allEntities conn = fold_ conn "SELECT * FROM entities" IM.empty f
  where
    f a r = return $ IM.insertWith (<>) (entityPostId r) (V.singleton r) a

allSentences :: Connection -> IO (IntMap (Vector Sentence))
allSentences conn = fold_ conn "SELECT * FROM sentence_sentiments" IM.empty f
  where
    f a r = return $ IM.insertWith (<>) (sentencePostId r) (V.singleton r) a

insertAllPosts :: Foldable f => Connection -> f Post -> IO Int64
insertAllPosts conn posts = do
  let xs = toList posts
  executeMany conn insertPostQuery xs
  executeMany conn createStatusesQuery $ map (Only . postId) xs

insertEntities :: Foldable f => Connection -> f Entity -> IO Int64
insertEntities conn = executeMany conn insertEntityQuery . toList

remainingEntityPosts :: Connection -> IO [Post]
remainingEntityPosts conn = query_ conn remainingEntityPostsQuery

remainingSentimentPosts :: Connection -> IO [Post]
remainingSentimentPosts conn = query_ conn remainingSentimentPostsQuery

insertSentences :: Foldable f => Connection -> f Sentence -> IO Int64
insertSentences conn = executeMany conn insertSentenceQuery . toList

updateDocumentSentiment :: Connection -> (Double, Double, Post) -> IO Int64
updateDocumentSentiment conn (dss, dsm, post) = execute conn updateDocumentSentimentQuery (dss, dsm, postId post)

updateEntitiesStatus :: Connection -> Post -> IO Int64
updateEntitiesStatus conn p = execute conn updateEntitiesStatusQuery . Only $ postId p

updateSentimentStatus :: Connection -> Post -> IO Int64
updateSentimentStatus conn p = execute conn updateSentimentStatusQuery . Only $ postId p


-- Helpers

minecartConn :: IO Connection
minecartConn = connect $ defaultConnectInfo { connectDatabase = "minecart" }

ifEntries :: [a] -> IO Int64 -> IO Int64
ifEntries xs action = if not $ null xs then action else return 0
