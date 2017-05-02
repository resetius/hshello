{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.BoundedChan as BChan
import Control.Concurrent.Chan
import Text.HTML.TagSoup
import Text.StringLike

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

tagFilter :: (StringLike a) => Tag a -> Bool
tagFilter (TagOpen "a" attrs) =
  let r = lookup "href" attrs in
    case r of
      Nothing -> False
      _ -> True
tagFilter tag = False

tagHref :: (StringLike a) => Tag a -> String
tagHref (TagOpen "a" attrs) =
  let r = lookup "href" attrs in
    case r of
      Just a -> toString a
      _ -> "_"
tagHref tag = "_"

extractLinks :: (StringLike body) => Response body -> [String]
extractLinks response =
  let tags = parseTags $ getResponseBody response in
    map tagHref $ filter tagFilter tags

enrichLink :: String -> String -> String
enrichLink _ link@('h':'t':'t':'p':_) = link
enrichLink url link = url ++ link

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

downloader :: BoundedHttpQueue -> HttpQueue -> IO()
downloader input output = loop
  where
    loop = do
      cmd <- BChan.readChan input
      case cmd of
        DownloaderRequest { uri = u } -> do
          putStrLn $ "Downloading " ++ u
          request <- parseRequest u
          response <- httpLBS request
          putStrLn $ "The status code of " ++  u ++ " was " ++ show (getResponseStatusCode response)
          {-let tags = parseTags $ getResponseBody response
          forM_ tags print-}
          let links = map (enrichLink u) $ extractLinks response
          {-forM_ links print-}
          forM_ links $ writeChan output . DownloaderRequest
          loop
        Stop -> return ()

main :: IO ()
main = do
    let reqs = ["http://httpbin.org"]
    let ndownloaders = 1000
    executorInput <- newChan
    executorOutput <- BChan.newBoundedChan ndownloaders
    ctl <- newEmptyMVar
    forM_ [1 .. ndownloaders] (\_ -> forkIO $ downloader executorOutput executorInput)
    forkIO $ executor executorInput executorOutput ctl
    {-takeMVar ctl-}
    forM_ reqs $ writeChan executorInput . DownloaderRequest
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
