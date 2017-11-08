{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cloudcart.Types
import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.Csv                   (ToRecord)
import qualified Data.Csv                   as C
import qualified Data.Csv.Streaming         as S
import           Data.Monoid                ((<>))
import           Data.Text.Lazy             (Text)
import           Network.HTTP.Simple
import           Pipes
import           System.Environment         (getArgs, lookupEnv)
import           System.Exit                (exitFailure, exitSuccess)

-- Pipes and Google Requests

type GoogleReader = ReaderT GoogleConfig IO

storeEntities :: Foldable f => f Post -> Effect GoogleReader ()
storeEntities = storeGoogleData $ mkGooglePipe googleEntityRequest

storeSentiments :: Foldable f => f Post -> Effect GoogleReader ()
storeSentiments = storeGoogleData $ mkGooglePipe googleSentimentRequest

storeGoogleData :: (Foldable f, ToRecord a)
                => Pipe Post [a] GoogleReader ()
                -> f Post
                -> Effect GoogleReader ()
storeGoogleData googlePipe posts =
  each posts
    >-> googlePipe
    >-> writeToCsv

writeToCsv :: ToRecord a => Consumer [a] GoogleReader ()
writeToCsv = forever $ do
  config <- lift ask
  let newFile = outputPath config <> outputFile (requestType config)
  await >>= (liftIO . BL.appendFile newFile . C.encode)

outputFile :: GoogleRequestType -> String
outputFile EntitySentimentAnalysis = "gb-forum-entities.csv"
outputFile SentimentAnalysis       = "gb-forum-sentiments.csv"

mkGooglePipe :: ToRecord a
             => (Post -> GoogleReader [a])
             -> Pipe Post [a] GoogleReader ()
mkGooglePipe googleRequest = forever $ await >>= (lift . googleRequest) >>= yield

googleEntityRequest :: Post -> GoogleReader [Entity]
googleEntityRequest = transformGoogleResponse setPostId
  where
    setPostId n (Entities xs) = map (\e -> e { entityPostId = n }) xs

googleSentimentRequest :: Post -> GoogleReader [SentenceMeta]
googleSentimentRequest = transformGoogleResponse setSentenceData
  where
    setSentenceData n (Sentences _ dss dsm xs) = map (SentenceMeta n dss dsm) xs

transformGoogleResponse :: (FromJSON a, ToRecord b)
                        => (Int -> a -> [b])
                        -> Post
                        -> GoogleReader [b]
transformGoogleResponse transformF post = do
  res <- googleRequest post
  return $ transformF (postId post) res

googleRequest :: FromJSON a => Post -> GoogleReader a
googleRequest post = do
  config <- ask
  let pId   = postId post
      key   = apiKey config
      type' = requestTypeString $ requestType config
      body' = setRequestBodyJSON . GoogleRequestText $ body post
      rawR  = "POST https://language.googleapis.com/v1/documents:"
              <> type'
              <> "?key="
              <> key
  liftIO $ do
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
  let runner ef = runReaderT (runEffect ef) config
  in
  case reqType of
    EntitySentimentAnalysis -> runner $ storeEntities posts
    SentimentAnalysis       -> runner $ storeSentiments posts

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
