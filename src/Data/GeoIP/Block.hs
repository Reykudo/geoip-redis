{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Data.GeoIP.Block where

import qualified Data.ByteString.Char8 as BS
import Data.CSV.Conduit.Conversion
import Data.Map.Ordered
import Data.Text hiding (empty)
import qualified Data.Text as Text hiding (empty)
import Net.IPv4 (IPv4Range, decodeRange, encodeRange)
import Prelude hiding (lookup)
import Data.Int
import Data.GeoIP.Instances

data GeoIPBlock range = GeoIPBlock
  { network :: !range
  , geoname_id :: !Int64
  , registered_country_geoname_id ::  !(Maybe Int)
  , represented_country_geoname_id :: !(Maybe Int)
  , is_anonymous_proxy :: !Bool
  , is_satellite_provider :: !Bool
  , postal_code :: !(Maybe Text)
  , latitude :: !Double
  , longitude :: !Double
  , accuracy_radius :: !Int
  }
  deriving (Show, Eq)

instance (FromField range) => FromNamedRecordOrdered (GeoIPBlock range) where
  parseNamedRecordOrdered omap = do
    network <- lookupOrdered omap "network"
    geoname_id <- lookupOrdered omap "geoname_id"
    registered_country_geoname_id <- lookupOrdered omap "registered_country_geoname_id"
    represented_country_geoname_id <- lookupOrdered omap "represented_country_geoname_id"
    is_anonymous_proxy <- lookupOrdered omap "is_anonymous_proxy"
    is_satellite_provider <- lookupOrdered omap "is_satellite_provider"
    postal_code <- lookupOrdered omap "postal_code"
    latitude <- lookupOrdered omap "latitude"
    longitude <- lookupOrdered omap "longitude"
    accuracy_radius <- lookupOrdered omap "accuracy_radius"
    pure $
      GeoIPBlock
        { network
        , geoname_id
        , registered_country_geoname_id
        , represented_country_geoname_id
        , is_anonymous_proxy
        , is_satellite_provider
        , postal_code
        , latitude
        , longitude
        , accuracy_radius
        }

instance (ToField range) => ToNamedRecordOrdered (GeoIPBlock range) where
  toNamedRecordOrdered
    GeoIPBlock
      { network
      , geoname_id
      , registered_country_geoname_id
      , represented_country_geoname_id
      , is_anonymous_proxy
      , is_satellite_provider
      , postal_code
      , latitude
      , longitude
      , accuracy_radius
      } =
      empty
        |> ("network", toField network)
        |> ("geoname_id", toField geoname_id)
        |> ("registered_country_geoname_id", toField registered_country_geoname_id)
        |> ("represented_country_geoname_id", toField represented_country_geoname_id)
        |> ("is_anonymous_proxy", toField is_anonymous_proxy)
        |> ("is_satellite_provider", toField is_satellite_provider)
        |> ("postal_code", toField postal_code)
        |> ("latitude", toField latitude)
        |> ("longitude", toField longitude)
        |> ("accuracy_radius", toField accuracy_radius)
