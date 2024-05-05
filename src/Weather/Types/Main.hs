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
import GHC.Generics (Generic)

data Main = Main
  { temp :: Double,
    feelsLike :: Double,
    tempMin :: Double,
    tempMax :: Double,
    pressure :: Int,
    humidity :: Int,
    seaLevel :: Int,
    grndLevel :: Int
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
