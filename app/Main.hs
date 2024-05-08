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
import Server.Utils (clientErrToServerErr, collectionWeather)
import Weather (Weather, getWeather)

type API =
  "weather"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> Get '[JSON] Weather

api :: Proxy API
api = Proxy

server :: Cache (Double, Double) Weather -> Server API
server cache (Just lat) (Just lon) = do
  eitherWeather <- liftIO $ do
    maybeWeather <- Cache.lookup cache (lat, lon)
    case maybeWeather of
      Nothing -> do
        putStrLn "Received from api"
        getWeather (pure lat) (pure lon)
      Just w -> do
        putStrLn "Received from cache"
        pure $ pure w
  helperServer eitherWeather
server _ lat lon = liftIO (getWeather lat lon) >>= helperServer

helperServer :: Either ClientError Weather -> Handler Weather
helperServer (Left err) = throwError $ clientErrToServerErr err
helperServer (Right weather) = pure weather

app :: Cache (Double, Double) Weather -> Application
app = serve api . server

main :: IO ()
main = do
  cache :: Cache (Double, Double) Weather <- Cache.newCache Nothing

  eitherConfig <- getConfig
  case eitherConfig of
    Left e -> print e
    Right config -> do
      let port = configPort config
          locations = configLocations config
          updatePeriod = configUpdatePeriod config

      _ <- forkIO $ collectionWeather updatePeriod cache locations

      putStrLn $ "Server was started http://localhost/:" <> show port
      run port $ app cache
