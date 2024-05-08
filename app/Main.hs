{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Config (Config (..), getConfig)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Get,
    Handler,
    JSON,
    Proxy (..),
    QueryParam,
    Server,
    serve,
    throwError,
    type (:>),
  )
import Servant.Client (ClientError)
import Server.Receivers
  ( receivedWeatherFromAPI,
    receivedWeatherFromCache,
  )
import Server.Utils (clientErrToServerErr, collectionWeather)
import System.Clock (Clock (Realtime), TimeSpec (sec), diffTimeSpec, getTime)
import Weather (Weather, getWeather)

type API =
  "weather"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> Get '[JSON] Weather

api :: Proxy API
api = Proxy

findNeededKey ::
  (Double, Double) ->
  Double ->
  [(Double, Double)] ->
  Maybe (Double, Double)
findNeededKey (lat, lon) offset = find $ \(lat', lon') ->
  and
    [ lat' - offset <= lat,
      lat <= lat' + offset,
      lon' - offset <= lon,
      lon <= lon' + offset
    ]

server ::
  Maybe Double ->
  Maybe Integer ->
  Cache (Double, Double) (Weather, TimeSpec) ->
  Server API
server offsetLocations offsetTime cache (Just lat) (Just lon) = do
  eitherWeather <- liftIO $ do
    keys <- Cache.keys cache
    let (targetLat, targetLon) =
          maybe
            (lat, lon)
            (fromMaybe (lat, lon) . flip (findNeededKey (lat, lon)) keys)
            offsetLocations

    maybeWeather <- Cache.lookup cache (targetLat, targetLon)

    case maybeWeather of
      Nothing -> receivedWeatherFromAPI cache lat lon
      Just (w, t) -> do
        case offsetTime of
          Nothing -> receivedWeatherFromCache w
          Just ot -> do
            ct <- getTime Realtime
            let dt = toInteger (sec (diffTimeSpec t ct)) `div` 60
            if dt <= ot
              then receivedWeatherFromCache w
              else receivedWeatherFromAPI cache lat lon
  helperServer eitherWeather
server _ _ _ lat lon = liftIO (getWeather lat lon) >>= helperServer

helperServer :: Either ClientError a -> Handler a
helperServer (Left err) = throwError $ clientErrToServerErr err
helperServer (Right a) = pure a

app ::
  Maybe Double ->
  Maybe Integer ->
  Cache (Double, Double) (Weather, TimeSpec) ->
  Application
app offsetLocations offsetTime =
  serve api . server offsetLocations offsetTime

main :: IO ()
main = do
  cache <- Cache.newCache Nothing

  eitherConfig <- getConfig
  case eitherConfig of
    Left e -> print e
    Right config -> do
      let port = configPort config
          locations = configLocations config
          updatePeriod = configUpdatePeriod config
          offsetLocations = configOffsetLocations config
          offsetTime = congigOffsetTime config

      _ <- forkIO $ collectionWeather updatePeriod cache locations

      putStrLn $ "Server was started http://localhost/:" <> show port
      run port $ app offsetLocations offsetTime cache
