{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Weather where

import Network.HTTP.Client.TLS (newTlsManager)
import Servant (Get, JSON, Proxy (..), QueryParam, type (:>))
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientError,
    ClientM,
    Scheme (Https),
    client,
    mkClientEnv,
    runClientM,
  )
import System.Environment (getEnv)
import Weather.Type (Weather)

type WeatherAPI =
  "data"
    :> "2.5"
    :> "weather"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> QueryParam "units" String
    :> QueryParam "appid" String
    :> Get '[JSON] Weather

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

queryWeather ::
  Maybe Double -> -- lat
  Maybe Double -> -- lon
  Maybe String -> -- units
  Maybe String -> -- appid
  ClientM Weather
queryWeather = client weatherAPI

query :: String -> ClientM Weather
query apiKey =
  queryWeather
    (Just 59.3957)
    (Just 56.4605)
    (Just "metric")
    (Just apiKey)

runWeather :: IO (Either ClientError Weather)
runWeather = do
  apiKey <- getEnv "WEATHER_API_KEY"
  m <- newTlsManager
  let url = BaseUrl Https "api.openweathermap.org" 443 ""
  runClientM (query apiKey) $ mkClientEnv m url
