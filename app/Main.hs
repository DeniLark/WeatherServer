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
    JSON,
    Proxy (..),
    QueryParam,
    Server,
    serve,
    throwError,
    type (:>),
  )
import Server.Utils (clientErrToServerErr, collectionWeather)
import Weather (Weather, getWeather)

type API =
  "weather"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> Get '[JSON] Weather

api :: Proxy API
api = Proxy

server :: Server API
server lat lon = do
  eitherWeather <- liftIO $ getWeather lat lon
  case eitherWeather of
    Left err -> do
      throwError $ clientErrToServerErr err
    Right weather -> pure weather

app :: Application
app = serve api server

main :: IO ()
main = do
  cache :: Cache (Double, Double) Weather <- Cache.newCache Nothing

  eitherConfig <- getConfig
  case eitherConfig of
    Left e -> print e
    Right config -> do
      let port = configPort config
          locations = configLocations config

      _ <- forkIO $ collectionWeather cache locations

      putStrLn $ "Server was started http://localhost/:" <> show port
      run port app
