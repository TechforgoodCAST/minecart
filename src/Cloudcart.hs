{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cloudcart.Types
import           Control.Concurrent     (threadDelay)
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

storeGoogleResponses :: Foldable f => GoogleConfig -> f Post -> Effect IO ()
storeGoogleResponses config posts =
    each posts
      >-> googlePipe config
      >-> writeToCsv config

writeToCsv :: GoogleConfig -> Consumer [Entity] IO ()
writeToCsv config = forever $ do
  let newFile = outputPath config <> "gb-forum-entities.csv"
  await >>= (liftIO . BL.appendFile newFile . C.encode)

googlePipe :: GoogleConfig -> Pipe Post [Entity] IO ()
googlePipe config = forever $ await >>= (liftIO . googleRequest config) >>= yield

googleRequest :: GoogleConfig -> Post -> IO [Entity]
googleRequest config post = do
  let pId   = postId post
      key   = apiKey config
      rawR  = "POST https://language.googleapis.com/v1/documents:analyzeEntitySentiment?key=" <> key
      body' = setRequestBodyJSON . GoogleRequestText $ body post
  putStrLn $ "fetching data for: " ++ show pId
  req <- body' <$> parseRequest rawR
  res <- httpJSON req
  return . setPostId pId $ getResponseBody res

setPostId :: Int -> Entities -> [Entity]
setPostId n (Entities xs) = map (\e -> e { entityPostId = n }) xs

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

decodeForum :: IO ()
decodeForum = do
  csvPath <- getCsvPath
  apiKey  <- getApiKey
  let config = GoogleConfig apiKey EntitySentimentAnalysis csvPath
  x <- BL.readFile $ csvPath <> "gb-forum.csv"
  case S.decodeByName x of
    Left err         -> print err >> exitFailure
    Right (_, posts) -> do
      runEffect $ storeGoogleResponses config posts
      putStrLn "done"
      exitSuccess

main :: IO ()
main = decodeForum
