{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Data.GeoIP.Block where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Word

data GeoIPBlock range = GeoIPBlock
  { network :: !range 
  , geoname_id :: !Int64
  , registered_country_geoname_id :: !(Maybe Int)
  , represented_country_geoname_id :: !(Maybe Int)
  , is_anonymous_proxy :: !Bool
  , is_satellite_provider :: !Bool
  , postal_code :: !(Maybe Text)
  , latitude :: !Double
  , longitude :: !Double
  , accuracy_radius :: !Int
  }
  deriving (Show, Eq)
