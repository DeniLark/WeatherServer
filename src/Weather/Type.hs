{-# LANGUAGE DeriveGeneric #-}

module Weather.Type where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toEncoding),
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
  )
import GHC.Generics (Generic)
import Weather.Types.Clouds (Clouds)
import Weather.Types.Coord (Coord)
import Weather.Types.Main (Main)
import Weather.Types.Sys (Sys)
import Weather.Types.Weather (OneWeather)
import Weather.Types.Wind (Wind)

data Weather = Weather
  { coord :: Coord,
    weather :: [OneWeather],
    base :: String,
    wMain :: Main,
    visibility :: Int,
    wind :: Wind,
    clouds :: Clouds,
    dt :: Integer,
    sys :: Sys,
    timezone :: Int,
    wId :: Integer,
    name :: String,
    cod :: Int
  }
  deriving (Generic)

fieldModifier :: String -> String
fieldModifier "wId" = "id"
fieldModifier "wMain" = "main"
fieldModifier s = s

instance FromJSON Weather where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

instance ToJSON Weather where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }