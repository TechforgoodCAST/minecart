module Minecart.GoogleCloud where

import           Control.Monad                    (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Int                         ()
import           Data.Monoid                      ((<>))
import           Database.PostgreSQL.Simple.ToRow ()
import qualified Minecart.Database                as DB
import           Minecart.Types
import           Network.HTTP.Simple
import           Pipes

type GoogleReader = ReaderT GoogleConfig IO

run :: Foldable f => GoogleConfig -> f Post -> IO ()
run config posts =
  case requestType config of
    "analyzeSentiment"       -> runR $ collectSentiment posts
    "analyzeEntitySentiment" -> runR $ collectEntities posts
    _                        -> putStrLn "unrecognized option"
  where
    runR eff = runReaderT (runEffect eff) config

-- Pipes

collectSentiment :: Foldable f => f Post -> Effect GoogleReader ()
collectSentiment posts =
  each posts
    >-> requestSentiment
    >-> insertSentiment

collectEntities :: Foldable f => f Post -> Effect GoogleReader ()
collectEntities posts =
  each posts
    >-> requestEntities
    >-> insertEntities

insertSentiment :: Consumer (Post, Sentiment) GoogleReader ()
insertSentiment = forever $ do
  conn                         <- dbConn <$> lift ask
  (post, Sentiment dss dsm xs) <- await
  liftIO $ do
    DB.updateSentimentStatus conn post
    putStrLn $ "queried sentiment for postId: " ++ show (postId post)
    DB.ifEntries xs $ do
      DB.insertSentences conn xs
      DB.updateDocumentSentiment conn (dss, dsm, post)
    putStrLn $ "inserted sentences for postId: " ++ show (postId post)

insertEntities :: Consumer (Post, Entities) GoogleReader ()
insertEntities = forever $ do
  conn                <- dbConn <$> lift ask
  (post, Entities xs) <- await
  liftIO $ do
    putStrLn $ "queried entities for postId: " ++ show (postId post)
    DB.updateEntitiesStatus conn post
    DB.ifEntries xs $ do
      putStrLn $ "inserted entities for postId: " ++ show (postId post)
      DB.insertEntities conn xs

requestSentiment :: Pipe Post (Post, Sentiment) GoogleReader ()
requestSentiment = pipeReq $ googleRequest formatSentiment

requestEntities :: Pipe Post (Post, Entities) GoogleReader ()
requestEntities = pipeReq $ googleRequest formatEntities

pipeReq :: MonadIO m => (a -> m b) -> Pipe a b m ()
pipeReq request = forever $ await >>= (lift . request) >>= yield


-- Requests

googleRequest :: FromJSON a
              => (Post -> a -> b)
              -> Post
              -> GoogleReader (Post, b)
googleRequest format post = do
  config <- ask
  let setBody = setRequestBodyJSON . GoogleRequest $ body post
      baseUrl = "https://language.googleapis.com/v1/documents:"
      request = setBody <$> parseRequest rawReq
      rawReq  = "POST " <> baseUrl
                        <> requestType config
                        <> "?key="
                        <> apiKey config
  res <- (format post . getResponseBody) <$> (request >>= httpJSON)
  return (post, res)


-- Data

formatEntities :: Post -> Entities -> Entities
formatEntities p (Entities xs) = Entities $ map f xs
  where f e = e { entityPostId = postId p }

formatSentiment :: Post -> Sentiment -> Sentiment
formatSentiment p (Sentiment a b xs) = Sentiment a b $ map f xs
  where f s = s { sentencePostId = postId p }
