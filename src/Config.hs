{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    genericParseJSON,
  )
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)

data Location = Location
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show)

instance FromJSON Location

data Config = Config
  { configPort :: Int,
    configLocations :: Maybe [Location],
    configUpdatePeriod :: Maybe Integer, -- in minutes
    configOffsetLocations :: Maybe Double,
    congigOffsetTime :: Maybe Integer -- in minutes
  }
  deriving (Generic, Show)

fieldModifier :: String -> String
fieldModifier "configPort" = "port"
fieldModifier "configLocations" = "locations"
fieldModifier "configUpdatePeriod" = "updatePeriod"
fieldModifier "configOffsetLocations" = "offsetLocations"
fieldModifier "congigOffsetTime" = "offsetTime"
fieldModifier s = s

instance FromJSON Config where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

getConfig :: IO (Either ParseException Config)
getConfig = decodeFileEither "config.yaml"
