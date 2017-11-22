module Minecart where

import Minecart.Core
import System.Console.GetOpt
import System.Environment

data Flag =
    PgSetup
  | CollectEntities
  | CollectSentences
  | ElasticsearchSetup
  | ElasticsearchIndex
  deriving Show

options :: [OptDescr Flag]
options =
  [ Option [] ["pgsetup"]      (NoArg PgSetup) "sets up intermediate postgres db from gb-forum.csv"
  , Option [] ["entities"]     (NoArg CollectEntities) "collects entities from google cloud for each post"
  , Option [] ["sentences"]    (NoArg CollectSentences) "collects sentence sentiments from google cloud for each post"
  , Option [] ["index"]        (NoArg ElasticsearchIndex) "indexes the collected data in elasticsearch"
  , Option [] ["elasticsetup"] (NoArg ElasticsearchSetup) "sets up elasticsearch index"
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
    PgSetup            -> setup
    CollectEntities    -> collectEntities
    CollectSentences   -> collectSentences
    ElasticsearchIndex -> postToElasticsearch
    ElasticsearchSetup -> resetIndex

app :: IO ()
app = (currentOption <$> getArgs) >>= either putStrLn action
