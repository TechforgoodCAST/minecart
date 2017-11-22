module Minecart where

import Minecart.Core
import System.Console.GetOpt
import System.Environment

data Flag =
    Setup
  | CollectEntities
  | CollectSentences
  | PostToElasticSearch
  | ResetIndex
  deriving Show

options :: [OptDescr Flag]
options =
  [ Option [] ["setup"]       (NoArg Setup) "sets up intermediate postgres db from gb-forum.csv"
  , Option [] ["entities"]    (NoArg CollectEntities) "collects entities from google cloud for each post"
  , Option [] ["sentences"]   (NoArg CollectSentences) "collects sentence sentiments from google cloud for each post"
  , Option [] ["post"]        (NoArg PostToElasticSearch) "posts the collected data to elasticsearch"
  , Option [] ["reset-index"] (NoArg ResetIndex) "resets elasticsearch index"
  ]

currentOption :: [String] -> Either String Flag
currentOption argv =
  case getOpt Permute options argv of
    ([], _,    _) -> Left info
    (opts, _, []) -> Right $ head opts
    (_, _,  errs) -> Left  $ concat errs ++ info

info :: String
info = usageInfo "Usage: " options

action :: Flag -> IO ()
action f =
  case f of
    Setup               -> setup
    CollectEntities     -> collectEntities
    CollectSentences    -> collectSentences
    PostToElasticSearch -> postToElasticsearch
    ResetIndex          -> resetIndex

app :: IO ()
app = (currentOption <$> getArgs) >>= either putStrLn action
