{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module Data.GeoIP.Location where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either.Combinators (rightToMaybe)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Store
import qualified Data.Store as Store
import Data.Text (Text)
import Data.Word
import Database.Redis
import GHC.Generics (Generic)

data GeoIPLocation = GeoIPLocation
  { geoname_id :: !Int64
  , locale_code :: !(Maybe Text)
  , continent_code :: !(Maybe Text)
  , continent_name :: !(Maybe Text)
  , country_iso_code :: !(Maybe Text)
  , country_name :: !(Maybe Text)
  , subdivision_1_iso_code :: !(Maybe Text)
  , subdivision_1_name :: !(Maybe Text)
  , subdivision_2_iso_code :: !(Maybe Text)
  , subdivision_2_name :: !(Maybe Text)
  , city_name :: !(Maybe Text)
  , metro_code :: !(Maybe Text)
  , time_zone :: !(Maybe Text)
  , is_in_european_union :: !Bool
  }
  deriving (Generic, Show, Eq, Store)

data WithIPBorders = WithIPBorders
  { lowerBorder :: Word64
  , upperBorder :: Word64
  , location :: GeoIPLocation
  }
  deriving (Generic, Show, Eq, Store)

-- type
lookupGeoByIP :: ByteString -> Word64 -> Redis (Maybe WithIPBorders)
lookupGeoByIP db_key ip = do
  res <- zrevrangebyscoreLimit @_ @(Either Reply) db_key (fromIntegral ip) 0.0 0 1
  pure $ rightToMaybe res >>= listToMaybe >>= rightToMaybe . Store.decode @WithIPBorders

-- aaa :: IO (Maybe WithIPBorders)
-- aaa = do
--   let Right c = parseConnectInfo $ "redis://user@localhost:6379/0"
--   con <- connect c
--   runRedis con $ lookupGeoByIP "geoip_ipv4" 1845250916