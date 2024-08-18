{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Config (Config (..), getConfig)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Swagger (Swagger)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
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
    type (:<|>) (..),
    type (:>),
  )
import Servant.Client (ClientError)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI
  ( SwaggerSchemaUI,
    swaggerSchemaUIServer,
  )
import Server.Receivers
  ( receivedWeatherFromAPI,
    receivedWeatherFromCache,
  )
import Server.Utils (clientErrToServerErr, collectionWeather)
import System.Clock (Clock (Realtime), TimeSpec (sec), diffTimeSpec, getTime)
import Weather (Weather, getWeather)

type WeatherAPI =
  "weather"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> Get '[JSON] Weather

type API =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> WeatherAPI

api :: Proxy API
api = Proxy

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy WeatherAPI)

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
  Manager ->
  Maybe Double ->
  Maybe Integer ->
  Cache (Double, Double) (Weather, TimeSpec) ->
  Server API
server tlsManager offsetLocations offsetTime cache =
  swaggerSchemaUIServer swaggerDoc
    :<|> serverWeather tlsManager offsetLocations offsetTime cache

serverWeather ::
  Manager ->
  Maybe Double ->
  Maybe Integer ->
  Cache (Double, Double) (Weather, TimeSpec) ->
  Maybe Double ->
  Maybe Double ->
  Handler Weather
serverWeather tlsManager offsetLocations offsetTime cache (Just lat) (Just lon) = do
  eitherWeather <- liftIO $ do
    keys <- Cache.keys cache
    let (targetLat, targetLon) =
          maybe
            (lat, lon)
            (fromMaybe (lat, lon) . flip (findNeededKey (lat, lon)) keys)
            offsetLocations

    maybeWeather <- Cache.lookup cache (targetLat, targetLon)

    case maybeWeather of
      Nothing -> receivedWeatherFromAPI tlsManager cache lat lon
      Just (w, t) -> do
        case offsetTime of
          Nothing -> receivedWeatherFromCache w
          Just ot -> do
            ct <- getTime Realtime
            let dt = toInteger (sec (diffTimeSpec t ct)) `div` 60
            if dt <= ot
              then receivedWeatherFromCache w
              else receivedWeatherFromAPI tlsManager cache lat lon
  helperServer eitherWeather
serverWeather tlsManager _ _ _ lat lon = liftIO (getWeather tlsManager lat lon) >>= helperServer

helperServer :: Either ClientError a -> Handler a
helperServer (Left err) = throwError $ clientErrToServerErr err
helperServer (Right a) = pure a

app ::
  Manager ->
  Maybe Double ->
  Maybe Integer ->
  Cache (Double, Double) (Weather, TimeSpec) ->
  Application
app tlsManager offsetLocations offsetTime =
  serve api . server tlsManager offsetLocations offsetTime

main :: IO ()
main = do
  cache <- Cache.newCache Nothing
  tlsManager <- newTlsManager

  eitherConfig <- getConfig
  case eitherConfig of
    Left e -> print e
    Right config -> do
      let port = configPort config
          locations = concat $ configLocations config
          updatePeriod = configUpdatePeriod config
          offsetLocations = configOffsetLocations config
          offsetTime = congigOffsetTime config
          actionFillCache =
            collectionWeather tlsManager updatePeriod cache locations
          actionServer = do
            putStrLn $ "Server was started http://localhost:" <> show port
            run port $ app tlsManager offsetLocations offsetTime cache

      runConcurrently $ Concurrently actionServer <|> Concurrently actionFillCache
