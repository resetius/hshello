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
import Data.Maybe
import Network.HTTP.Client

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
tagFilter (TagOpen "a" attrs) = isJust $ lookup "href" attrs
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

protoPrefix :: Request -> String
protoPrefix r =
  if secure r then
    "https://"
  else
    "http://"

enrichLink :: Request -> String -> String
enrichLink _ link@('h':'t':'t':'p':_) = link
enrichLink r link@('/':_) =
  protoPrefix r ++ toString (host r) ++ ":" ++ show (port r) ++ link
enrichLink r link =
  protoPrefix r ++ toString (host r) ++ ":" ++ show (port r) ++ toString (path r) ++ link

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
          {- TODO: check u already downloaded here -}
          {- TODO: filter links here (local links/external links/blacklisted and so on)-}
          putStrLn $ "Downloading " ++ u
          request <- parseRequest u
          response <- httpLBS request
          putStrLn $ "The status code of " ++  u ++ " was " ++ show (getResponseStatusCode response)
          {-let tags = parseTags $ getResponseBody response
          forM_ tags print-}
          let links = map (enrichLink request) $ extractLinks response
          {-forM_ links print-}
          forM_ links $ writeChan output . DownloaderRequest
          loop
        Stop -> return ()

waitLoop :: HttpQueue -> BoundedHttpQueue -> IO()
waitLoop input output = do
  empty1 <- isEmptyChan input
  empty2 <- BChan.isEmptyChan output
  if empty1 && empty2 then
    return ()
  else
    waitLoop input output

main :: IO ()
main = do
    let reqs = ["http://localhost:8083"]
    let ndownloaders = 1000
    executorInput <- newChan
    executorOutput <- BChan.newBoundedChan (10*ndownloaders)
    ctl <- newEmptyMVar
    forM_ [1 .. ndownloaders] (\_ -> forkIO $ downloader executorOutput executorInput)
    forkIO $ executor executorInput executorOutput ctl
    {-takeMVar ctl-}
    forM_ reqs $ writeChan executorInput . DownloaderRequest
    {-threadDelay 1000-}
    waitLoop executorInput executorOutput
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
