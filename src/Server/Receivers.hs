module Server.Receivers where

import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Network.HTTP.Client (Manager)
import Servant.Client (ClientError)
import System.Clock (TimeSpec, getTime)
import System.Clock.Seconds (Clock (Realtime))
import Weather (Weather, getWeather)

receivedWeatherFromAPI ::
  Manager ->
  Cache (Double, Double) (Weather, TimeSpec) ->
  Double ->
  Double ->
  IO (Either ClientError Weather)
receivedWeatherFromAPI tlsManager cache lat lon = do
  putStrLn "Received from api"
  eitherWeather <- getWeather tlsManager (pure lat) (pure lon)

  case eitherWeather of
    Left err -> pure $ Left err
    Right w -> do
      t <- getTime Realtime
      Cache.insert cache (lat, lon) (w, t)
      pure $ pure w

receivedWeatherFromCache :: Weather -> IO (Either ClientError Weather)
receivedWeatherFromCache w = putStrLn "Received from cache" >> pure (pure w)