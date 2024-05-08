{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Sys where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)

data Sys = Sys
  { country :: Maybe String,
    sunrise :: Maybe Integer,
    sunset :: Maybe Integer
  }
  deriving (Generic)

instance ToSchema Sys

instance FromJSON Sys

instance ToJSON Sys
