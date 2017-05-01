{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.BoundedChan as BChan
import Control.Concurrent.Chan

data Message = DownloaderRequest {
  uri :: String
} | DownloaderResponse {
  uri :: String
} | Stop deriving (Show)

type HttpQueue = Chan Message
type BoundedHttpQueue = BChan.BoundedChan Message


{- scheme:
uri -> executor -> [[ downloaders ]] -> [[parsers]] ->
         ^                                           |
         |                                           V
         ^-------------------------------------------<

executor -> Control.Concurrent.BoundedChan -> [[ downloaders ]]
[[parsers]] -> Control.Concurrent.Chan -> executor
-}

executor :: HttpQueue -> BoundedHttpQueue -> MVar Message -> IO ()
executor input output ctl = loop
  where
    loop = do
      cmd <- readChan input
      case cmd of
        DownloaderRequest { uri = _ } -> do
          BChan.writeChan output cmd
          loop
        Stop -> do
          putMVar ctl cmd
          return ()

downloader :: BoundedHttpQueue -> IO()
downloader input = loop
  where
    loop = do
      cmd <- BChan.readChan input
      case cmd of
        DownloaderRequest { uri = u } -> do
          request <- parseRequest u
          response <- httpLBS request
          putStrLn $ "The status code of " ++  u ++ " was " ++ show (getResponseStatusCode response)
          loop
        Stop -> return ()

main :: IO ()
main = do
    let reqs = ["http://httpbin.org/get", "http://httpbin.org/get", "http://google.com"]
    let ndownloaders = 1000
    executorInput <- newChan
    executorOutput <- BChan.newBoundedChan ndownloaders
    ctl <- newEmptyMVar
    forM_ [1 .. ndownloaders] (\_ -> forkIO $ downloader executorOutput)
    forkIO $ executor executorInput executorOutput ctl
    {-takeMVar ctl-}
    forM_ reqs (\r -> writeChan executorInput $ DownloaderRequest r)
    threadDelay 100000000
    writeChan executorInput Stop
    {-executor executorInput executorOutput ctl-}
    {-forkIO $ executor executorInput executorOutput ctl-}
    takeMVar ctl
    return ()

    {-
    responses <- mapM httpLBS reqs

    forM_ responses (\response -> do
      putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
      print $ getResponseHeader "Content-Type" response
      L8.putStrLn $ getResponseBody response)
    return ()
-}
