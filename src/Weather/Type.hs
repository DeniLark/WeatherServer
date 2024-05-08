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
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Weather.Types.Clouds (Clouds)
import Weather.Types.Coord (Coord)
import Weather.Types.Main (Main)
import Weather.Types.Sys (Sys)
import Weather.Types.Weather (OneWeather)
import Weather.Types.Wind (Wind)

data Weather = Weather
  { coord :: Maybe Coord,
    weather :: Maybe [OneWeather],
    base :: Maybe String,
    wMain :: Maybe Main,
    visibility :: Maybe Int,
    wind :: Maybe Wind,
    clouds :: Maybe Clouds,
    dt :: Maybe Integer,
    sys :: Maybe Sys,
    timezone :: Maybe Int,
    wId :: Maybe Integer,
    name :: Maybe String,
    cod :: Maybe Int
  }
  deriving (Generic)

fieldModifier :: String -> String
fieldModifier "wId" = "id"
fieldModifier "wMain" = "main"
fieldModifier s = s

instance ToSchema Weather

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