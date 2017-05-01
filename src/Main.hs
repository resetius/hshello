{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import Control.Monad
import Control.Concurrent
import Control.Concurrent.BoundedChan
import Control.Concurrent.Chan

data HttpTask = HttpTask {
  uri :: String,
  startTime:: Int } | HttpStop deriving (Show)

newtype HttpQueue = HttpQueue (MVar HttpTask)

newHttpQueue :: IO HttpQueue
newHttpQueue = do
  m <- newEmptyMVar
  let h = HttpQueue m
  return h

{- scheme:
uri -> executor -> [[ doanloaders ]] -> [[parsers]] ->
         ^                                           |
         |                                           V
         ^-------------------------------------------<

executor -> Control.Concurrent.BoundedChan -> [[ downloaders ]]
[[parsers]] -> Control.Concurrent.Chan -> executor
-}

{-
 TODO: rename -> executor
 -}
download :: HttpQueue -> IO ()
download (HttpQueue x) = loop
  where
    loop = do
      cmd <- takeMVar x
      case cmd of
        HttpTask { uri = u, startTime = _ } -> do
          putStrLn u
          loop
        HttpStop -> return ()

main :: IO ()
main = do
    let reqs = ["http://httpbin.org/get", "http://httpbin.org/get", "http://google.com"]
    queue <- newHttpQueue
    return ()

    {-
    responses <- mapM httpLBS reqs

    forM_ responses (\response -> do
      putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
      print $ getResponseHeader "Content-Type" response
      L8.putStrLn $ getResponseBody response)
    return ()
-}
