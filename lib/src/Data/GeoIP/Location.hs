{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import qualified Data.Text.Encoding as Text
import Data.Word
import Database.Redis
import GHC.Generics (Generic)

data GeoIPLocation1 str = GeoIPLocation
  { geoname_id :: !Int64
  , locale_code :: !(Maybe str)
  , continent_code :: !(Maybe str)
  , continent_name :: !(Maybe str)
  , country_iso_code :: !(Maybe str)
  , country_name :: !(Maybe str)
  , subdivision_1_iso_code :: !(Maybe str)
  , subdivision_1_name :: !(Maybe str)
  , subdivision_2_iso_code :: !(Maybe str)
  , subdivision_2_name :: !(Maybe str)
  , city_name :: !(Maybe str)
  , metro_code :: !(Maybe str)
  , time_zone :: !(Maybe str)
  , is_in_european_union :: !Bool
  }
  deriving (Generic, Show, Eq, Functor)

type GeoIPLocation = GeoIPLocation1 Text

data WithIPBorders1 str = WithIPBorders
  { lowerBorder :: Word64
  , upperBorder :: Word64
  , location :: GeoIPLocation1 str
  }
  deriving (Generic, Show, Eq, Functor)

deriving instance Store (GeoIPLocation1 ByteString)
deriving instance Store (WithIPBorders1 ByteString)

type WithIPBorders = WithIPBorders1 Text

-- type
lookupGeoByIP :: ByteString -> Word64 -> Redis (Maybe WithIPBorders)
lookupGeoByIP db_key ip = do
  res <- zrevrangebyscoreLimit @_ @(Either Reply) db_key (fromIntegral ip) 0.0 0 1
  pure $ rightToMaybe res >>= listToMaybe >>= rightToMaybe . fmap (fmap (Text.decodeUtf8)) . Store.decode @(WithIPBorders1 ByteString)

-- aaa :: IO (Maybe WithIPBorders)
-- aaa = do
--   let Right c = parseConnectInfo $ "redis://user@localhost:6379/0"
--   con <- connect c
--   runRedis con $ lookupGeoByIP "geoip_ipv4" 1845250916