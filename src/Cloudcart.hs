{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Client
import Pipes

xs :: Manager -> Request -> Consumer Int IO ()
xs manager request = forever $ do
  x <- await
  liftIO $ do
    res <- httpLbs request manager
    print x
    print $ responseBody res
    threadDelay $ 1000000 * 2


main :: IO ()
main = do
  manager  <- newManager defaultManagerSettings
  request  <- parseRequest "http://httpbin.org/ip"
  runEffect $ each [1..10] >-> xs manager request
