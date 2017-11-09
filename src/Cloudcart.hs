{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cloudcart.Types
import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.Csv                   (DefaultOrdered, ToNamedRecord)
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

storeGoogleData :: (Foldable f, ToNamedRecord a, DefaultOrdered a)
                => Pipe Post [a] GoogleReader ()
                -> f Post
                -> Effect GoogleReader ()
storeGoogleData googlePipe posts =
  each posts
    >-> googlePipe
    >-> writeToCsv

writeToCsv :: (ToNamedRecord a, DefaultOrdered a) => Consumer [a] GoogleReader ()
writeToCsv = forever $ do
  config <- lift ask
  let newFile = outputPath config <> outputFile (requestType config)
  await >>= (liftIO . BL.appendFile newFile . C.encodeDefaultOrderedByName)

outputFile :: GoogleRequestType -> String
outputFile EntityAnalysis    = "gb-forum-entities.csv"
outputFile SentimentAnalysis = "gb-forum-sentiments.csv"

mkGooglePipe :: ToNamedRecord a
             => (Post -> GoogleReader [a])
             -> Pipe Post [a] GoogleReader ()
mkGooglePipe googleRequest = forever $ await >>= (lift . googleRequest) >>= yield

googleEntityRequest :: Post -> GoogleReader [Entity]
googleEntityRequest = transformGoogleResponse setPostId
  where
    setPostId n (Entities xs) = map (\e -> e { entityPostId = n }) xs

googleSentimentRequest :: Post -> GoogleReader [SentenceSentiment]
googleSentimentRequest = transformGoogleResponse setSentenceData
  where
    setSentenceData n (RawSentimentData _ dss dsm xs) =
      map (\(RawSentence s sss ssm) -> SentenceSentiment n dss dsm s sss ssm) xs

transformGoogleResponse :: (FromJSON a, ToNamedRecord b, DefaultOrdered b)
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
requestTypeString EntityAnalysis    = "analyzeEntitySentiment"
requestTypeString SentimentAnalysis = "analyzeSentiment"


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

requestHandler :: Foldable f => f Post -> Effect GoogleReader ()
requestHandler posts = do
  reqType <- requestType <$> lift ask
  case reqType of
    EntityAnalysis    -> storeEntities posts
    SentimentAnalysis -> storeSentiments posts

collectGoogleCloudData :: IO ()
collectGoogleCloudData = do
  csvPath <- getCsvPath
  apiKey  <- getApiKey
  forum   <- BL.readFile $ csvPath <> "gb-forum.csv"
  let config    = GoogleConfig apiKey SentimentAnalysis csvPath
      runner ef = runReaderT (runEffect ef) config
  case S.decodeByName forum of
    Left err         -> print err >> exitFailure
    Right (_, posts) -> do
      runner $ requestHandler posts
      putStrLn "done"
      exitSuccess

main :: IO ()
main = collectGoogleCloudData
