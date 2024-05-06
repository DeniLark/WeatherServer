{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Clouds where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Clouds = Clouds {all :: Maybe Int}
  deriving (Generic)

instance FromJSON Clouds

instance ToJSON Clouds