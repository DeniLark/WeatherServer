{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Sys where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Sys = Sys
  { country :: String,
    sunrise :: Integer,
    sunset :: Integer
  }
  deriving (Generic)

instance FromJSON Sys

instance ToJSON Sys
