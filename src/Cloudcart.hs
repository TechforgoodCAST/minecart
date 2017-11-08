{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cloudcart.Types
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, mapM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Csv               as C
import qualified Data.Csv.Streaming     as S
import           Data.Foldable          (toList)
import           Data.Monoid            ((<>))
import           Data.Text.Lazy         (Text)
import           Network.HTTP.Simple
import           Pipes
import           System.Environment     (getArgs, lookupEnv)
import           System.Exit

wholeShebang :: Foldable f => String -> f Post -> Effect IO ()
wholeShebang apiKey posts = each posts >-> googlePipe apiKey >-> writeToCsv

writeToCsv :: Consumer Entities IO ()
writeToCsv = forever $ do
  (Entities e) <- await
  liftIO . BL.appendFile "./new-data.csv" $ C.encode e

googlePipe :: String -> Pipe Post Entities IO ()
googlePipe apiKey = forever $ await >>= (liftIO . googleRequest apiKey) >>= yield

googleRequest :: String -> Post -> IO Entities
googleRequest apiKey post = do
  let rawR  = "POST https://language.googleapis.com/v1/documents:analyzeEntitySentiment?key=" <> apiKey
      body' = setRequestBodyJSON . GoogleRequest $ body post
  req <- body' <$> parseRequest rawR
  res <- httpJSON req
  return . setPostId (postId post) $ getResponseBody res

setPostId :: Int -> Entities -> Entities
setPostId n (Entities xs) = Entities $ map (\e -> e { entityPostId = n }) xs

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
  x <- BL.readFile $ csvPath <> "gb-forum.csv"
  case S.decodeByName x of
    Left err         -> print err
    Right (_, posts) -> runEffect . wholeShebang apiKey . take 2 . toList $ posts

main :: IO ()
main = decodeForum
