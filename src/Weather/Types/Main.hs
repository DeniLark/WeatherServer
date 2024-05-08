{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Main where

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

data Main = Main
  { temp :: Maybe Double,
    feelsLike :: Maybe Double,
    tempMin :: Maybe Double,
    tempMax :: Maybe Double,
    pressure :: Maybe Int,
    humidity :: Maybe Int,
    seaLevel :: Maybe Int,
    grndLevel :: Maybe Int
  }
  deriving (Generic)

fieldModifier :: String -> String
fieldModifier "feelsLike" = "feels_like"
fieldModifier "tempMin" = "temp_min"
fieldModifier "tempMax" = "temp_max"
fieldModifier "seaLevel" = "sea_level"
fieldModifier "grndLevel" = "grnd_level"
fieldModifier s = s

instance FromJSON Main where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

instance ToJSON Main where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

instance ToSchema Main