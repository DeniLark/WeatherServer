{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Yaml (FromJSON, ParseException, decodeFileEither)
import GHC.Generics (Generic)

newtype Config = Config
  { port :: Int
  }
  deriving (Generic, Show)

instance FromJSON Config

getConfig :: IO (Either ParseException Config)
getConfig = decodeFileEither "config.yaml"