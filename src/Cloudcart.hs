{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cloudcart.Types
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Csv               as C
import qualified Data.Csv.Streaming     as S
import           Data.Monoid            ((<>))
import           Data.Text.Lazy         (Text)
import           Network.HTTP.Simple
import           Pipes
import           System.Environment     (getArgs, lookupEnv)
import           System.Exit            (exitFailure, exitSuccess)

-- Pipes and Google Requests

type GooglePipe a = Pipe Post [a] IO ()

storeEntities :: Foldable f => GoogleConfig -> f Post -> Effect IO ()
storeEntities = storeGoogleData $ mkGooglePipe googleEntityRequest

storeSentiments :: Foldable f => GoogleConfig -> f Post -> Effect IO ()
storeSentiments = storeGoogleData $ mkGooglePipe googleSentimentRequest

storeGoogleData :: (Foldable f, C.ToRecord a)
                => (GoogleConfig -> GooglePipe a)
                -> GoogleConfig
                -> f Post
                -> Effect IO ()
storeGoogleData googlePipe config posts =
  each posts
    >-> googlePipe config
    >-> writeToCsv config

writeToCsv :: C.ToRecord a => GoogleConfig -> Consumer [a] IO ()
writeToCsv config = forever $ do
  let newFile = outputPath config <> outputFile (requestType config)
  await >>= (liftIO . BL.appendFile newFile . C.encode)

outputFile :: GoogleRequestType -> String
outputFile EntitySentimentAnalysis = "gb-forum-entities.csv"
outputFile SentimentAnalysis       = "gb-forum-sentiments.csv"

mkGooglePipe :: C.ToRecord a
             => (GoogleConfig -> Post -> IO [a])
             -> GoogleConfig
             -> GooglePipe a
mkGooglePipe googleRequest config = forever $ await >>= (liftIO . googleRequest config) >>= yield

googleEntityRequest :: GoogleConfig -> Post -> IO [Entity]
googleEntityRequest = transformGoogleResponse setPostId
  where
    setPostId n (Entities xs) = map (\e -> e { entityPostId = n }) xs

googleSentimentRequest :: GoogleConfig -> Post -> IO [SentenceMeta]
googleSentimentRequest = transformGoogleResponse setSentenceData
  where
    setSentenceData n (Sentences _ dss dsm xs) = map (SentenceMeta n dss dsm) xs

transformGoogleResponse :: (FromJSON a, C.ToRecord b) => (Int -> a -> [b]) -> GoogleConfig -> Post -> IO [b]
transformGoogleResponse transformF config post = do
  res <- googleRequest config post
  return $ transformF (postId post) res

googleRequest :: FromJSON a => GoogleConfig -> Post -> IO a
googleRequest config post = do
  let pId   = postId post
      key   = apiKey config
      type' = requestTypeString $ requestType config
      body' = setRequestBodyJSON . GoogleRequestText $ body post
      rawR  = "POST https://language.googleapis.com/v1/documents:"
              <> type'
              <> "?key="
              <> key
  putStrLn $ "fetching data for: " ++ show pId
  req <- body' <$> parseRequest rawR
  res <- httpJSON req
  return $ getResponseBody res

requestTypeString :: GoogleRequestType -> String
requestTypeString EntitySentimentAnalysis = "analyzeEntitySentiment"
requestTypeString SentimentAnalysis       = "analyzeSentiment"


-- CLI helpers

getApiKey :: IO String
getApiKey = do
  mKey <- lookupEnv "GOOGLE_CLOUD_API_KEY"
  case mKey of
    Just key -> return key
    Nothing  -> do
      putStrLn "Please set GOOGLE_CLOUD_API_KEY environment variable"
      exitFailure

getCsvPath :: IO String
getCsvPath = do
  args <- getArgs
  if null args then do
    putStrLn "Please pass the path to the gb-forum.csv file as a command line arg"
    exitFailure
  else
    return $ head args

requestHandler :: Foldable f => GoogleConfig -> f Post -> IO ()
requestHandler config@(GoogleConfig _ reqType _) posts =
  case reqType of
    EntitySentimentAnalysis -> runEffect $ storeEntities config posts
    SentimentAnalysis       -> runEffect $ storeSentiments config posts

collectGoogleCloudData :: IO ()
collectGoogleCloudData = do
  csvPath <- getCsvPath
  apiKey  <- getApiKey
  let config = GoogleConfig apiKey SentimentAnalysis csvPath
  x <- BL.readFile $ csvPath <> "gb-forum.csv"
  case S.decodeByName x of
    Left err         -> print err >> exitFailure
    Right (_, posts) -> do
      requestHandler config posts
      putStrLn "done"
      exitSuccess

main :: IO ()
main = collectGoogleCloudData
