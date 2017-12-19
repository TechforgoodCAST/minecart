module Minecart where

import Minecart.Core
import System.Console.GetOpt
import System.Environment

data Flag =
    PgSetup
  | CollectEntities
  | CollectSentences
  | ElasticsearchIndex
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option [] ["pgsetup"]      (NoArg PgSetup) "sets up intermediate postgres db from gb-forum.csv"
  , Option [] ["entities"]     (NoArg CollectEntities) "collects entities from google cloud for each post"
  , Option [] ["sentences"]    (NoArg CollectSentences) "collects sentence sentiments from google cloud for each post"
  , Option [] ["elastic"]      (NoArg ElasticsearchIndex) "indexes the collected data in elasticsearch"
  ]

currentOption :: [String] -> Either String Flag
currentOption argv =
  case getOpt Permute options argv of
    ([], _,    _) -> Left info
    (opts, _, []) -> Right $ head opts

info :: String
info = usageInfo "Usage: " options

action :: Flag -> IO ()
action f =
  case f of
    PgSetup            -> setup
    CollectEntities    -> collectEntities
    CollectSentences   -> collectSentences
    ElasticsearchIndex -> resetIndex >> postToElasticsearch

app :: IO ()
app = (currentOption <$> getArgs) >>= either putStrLn action
