{-# LANGUAGE DeriveGeneric #-}

module Weather.Types.Wind where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Wind = Wind
  { speed :: Double,
    deg :: Int,
    gust :: Double
  }
  deriving (Generic)

instance FromJSON Wind

instance ToJSON Wind
