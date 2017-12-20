{-# LANGUAGE OverloadedStrings #-}

module Helper where

import           Data.Csv
import           Data.Time      (fromGregorian)
import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           Minecart.Types

csvHeaders :: Vector Name
csvHeaders = V.fromList
  [ "post_id"
  , "thread_id"
  , "user_id"
  , "body"
  , "date"
  , "visible"
  , "moderated"
  , "subject"
  , "username"
  , "forum_id"
  ]

samplePost :: Post
samplePost = Post
  { postId = 20295
  , threadId = 4756
  , userId = 1066420
  , username = "user1234"
  , subject = "hello I'm new here"
  , body = "This is a post body"
  , date = Just $ fromGregorian 2013 1 2
  , visible = True
  , moderated = True
  , documentSentimentScore = 0
  , documentSentimentMagnitude = 0
  , entities = V.empty
  , sentences = V.empty
  }
