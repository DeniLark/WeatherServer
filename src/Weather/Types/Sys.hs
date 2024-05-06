{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Sys where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Sys = Sys
  { country :: Maybe String,
    sunrise :: Maybe Integer,
    sunset :: Maybe Integer
  }
  deriving (Generic)

instance FromJSON Sys

instance ToJSON Sys
