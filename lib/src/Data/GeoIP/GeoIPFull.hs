{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GeoIP.GeoIPFull where

import Data.ByteString
import Data.Either.Combinators
import Data.GeoIP.Location
import Data.Maybe
import Data.Store
import qualified Data.Store as Store
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word
import Database.Redis
import GHC.Generics

data GeoIPFull1 str = GeoIPFull
  { lowerBorder :: Word64
  , upperBorder :: Word64
  , latitude :: !Double
  , longitude :: !Double
  , accuracy_radius :: !Int
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
  , postal_code :: !(Maybe str)
  , metro_code :: !(Maybe str)
  , time_zone :: !(Maybe str)
  , is_in_european_union :: !Bool
  }
  deriving (Generic, Show, Eq, Functor)

deriving instance Store (GeoIPFull1 ByteString)

type GeoIPFull = GeoIPFull1 Text

-- type
lookupGeoByIP :: ByteString -> Word64 -> Redis (Maybe GeoIPFull)
lookupGeoByIP db_key ip = do
  res <- zrevrangebyscoreLimit @_ @(Either Reply) db_key (fromIntegral ip) 0.0 0 1
  pure $ rightToMaybe res >>= listToMaybe >>= rightToMaybe . fmap (fmap (Text.decodeUtf8)) . Store.decode @(GeoIPFull1 ByteString)

-- aaa :: IO ()
-- aaa = do
--   let Right c = parseConnectInfo $ "redis://user@localhost:6379/0"
--   con <- connect c
--   runRedis con (lookupGeoByIP "geoip_ipv4" 1441407949) >>= pPrint 