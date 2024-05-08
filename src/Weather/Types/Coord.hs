{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Coord where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)

data Coord = Coord
  { lon :: Maybe Double,
    lat :: Maybe Double
  }
  deriving (Generic)

instance ToSchema Coord

instance FromJSON Coord

instance ToJSON Coord