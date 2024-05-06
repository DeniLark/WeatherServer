{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Coord where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Coord = Coord
  { lon :: Maybe Double,
    lat :: Maybe Double
  }
  deriving (Generic)

instance FromJSON Coord

instance ToJSON Coord