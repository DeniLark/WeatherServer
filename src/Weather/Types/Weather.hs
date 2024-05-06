{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Weather where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toEncoding),
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
  )
import GHC.Generics (Generic)

data OneWeather = OneWeather
  { wId :: Maybe Integer,
    main :: Maybe String,
    description :: Maybe String,
    icon :: Maybe String
  }
  deriving (Generic)

fieldModifier :: String -> String
fieldModifier "wId" = "id"
fieldModifier s = s

instance FromJSON OneWeather where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

instance ToJSON OneWeather where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }