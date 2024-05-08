{-# LANGUAGE OverloadedStrings #-}

module Server.Utils where

import Config (Location (Location))
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forM_)
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Maybe (fromMaybe)
import Servant (ServerError (errBody), err400)
import Servant.Client (ClientError (..), ResponseF (responseBody))
import Weather (Weather, getWeather)

collectionWeather ::
  Maybe Integer ->
  Cache (Double, Double) Weather ->
  [Location] ->
  IO ()
collectionWeather updatePeriod cache locations = do
  putStrLn "Collection weather"
  forM_ locations $ \(Location lat lon) -> do
    eitherWeather <- getWeather (pure lat) (pure lon)
    case eitherWeather of
      Left err ->
        putStrLn $
          "Error location " <> show lat <> ", " <> show lon <> ": "
            <> show err
      Right w -> do
        putStrLn $ show (lat, lon) <> ": insert into cache"
        Cache.insert cache (lat, lon) w
  delay $ fromMaybe 10 updatePeriod * 60000000 -- 60000000 = 1m
  collectionWeather updatePeriod cache locations

clientErrToServerErr :: ClientError -> ServerError
clientErrToServerErr (FailureResponse _ resp) =
  err400 {errBody = responseBody resp}
clientErrToServerErr (DecodeFailure _ resp) =
  err400 {errBody = responseBody resp}
clientErrToServerErr (UnsupportedContentType _ resp) =
  err400 {errBody = responseBody resp}
clientErrToServerErr (InvalidContentTypeHeader resp) =
  err400 {errBody = responseBody resp}
clientErrToServerErr (ConnectionError _) = err400 {errBody = "Unknown error"}
