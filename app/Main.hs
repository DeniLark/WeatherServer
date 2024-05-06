{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Config (Config (port), getConfig)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Weather

type API =
  "weather"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> Get '[JSON] Weather

api :: Proxy API
api = Proxy

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

server :: Server API
server lat lon = do
  eitherWeather <- liftIO $ runWeather lat lon
  case eitherWeather of
    Left err -> do
      throwError $ clientErrToServerErr err
    Right weather -> pure weather

app :: Application
app = serve api server

main :: IO ()
main = do
  eitherConfig <- getConfig
  case eitherConfig of
    Left e -> print e
    Right config -> do
      let p = port config
      putStrLn $ "Server was started http://localhost/:" <> show p
      run p app
