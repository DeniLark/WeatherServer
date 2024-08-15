{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Weather
  ( module Weather.Type,
    getWeather,
  )
where

import Control.Applicative ((<|>))
import Network.HTTP.Client (Manager)
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
  "2.5"
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

query :: String -> Maybe Double -> Maybe Double -> ClientM Weather
query apiKey lat lon =
  queryWeather
    lat
    lon
    (Just "metric")
    (Just apiKey)

getWeather :: Manager -> Maybe Double -> Maybe Double -> IO (Either ClientError Weather)
getWeather tlsManager lat lon = do
  apiKey <- getEnv "WEATHER_API_KEY"
  apiRoot <- getEnv "WEATHER_API_ROOT" <|> pure "data"

  let url = BaseUrl Https "api.openweathermap.org" 443 apiRoot
  runClientM (query apiKey lat lon) $ mkClientEnv tlsManager url