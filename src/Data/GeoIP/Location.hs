{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module Data.GeoIP.Location where

import Data.Bifunctor
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion
import Data.GeoIP.Instances
import Data.Int
import Data.Map.Ordered
import Data.Store
import Data.Text hiding (empty)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Data.Word
import Database.Redis
import GHC.Generics (Generic)
import Net.IPv4 (IPv4 (IPv4, getIPv4))
import Data.Maybe (listToMaybe)
import Data.CSV.Conduit.Parser.ByteString
import Data.ByteString.Char8 (ByteString)

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
  deriving (Generic, Show, Eq, Store, FromRecord)

instance FromNamedRecordOrdered GeoIPLocation where
  parseNamedRecordOrdered omap = do
    geoname_id <- lookupOrdered omap "geoname_id"
    locale_code <- lookupOrdered omap "locale_code"
    continent_code <- lookupOrdered omap "continent_code"
    continent_name <- lookupOrdered omap "continent_name"
    country_iso_code <- lookupOrdered omap "country_iso_code"
    country_name <- lookupOrdered omap "country_name"
    subdivision_1_iso_code <- lookupOrdered omap "subdivision_1_iso_code"
    subdivision_1_name <- lookupOrdered omap "subdivision_1_name"
    subdivision_2_iso_code <- lookupOrdered omap "subdivision_2_iso_code"
    subdivision_2_name <- lookupOrdered omap "subdivision_2_name"
    city_name <- lookupOrdered omap "city_name"
    metro_code <- lookupOrdered omap "metro_code"
    time_zone <- lookupOrdered omap "time_zone"
    is_in_european_union <- lookupOrdered omap "is_in_european_union"
    pure $
      GeoIPLocation
        { geoname_id
        , locale_code
        , continent_code
        , continent_name
        , country_iso_code
        , country_name
        , subdivision_1_iso_code
        , subdivision_1_name
        , subdivision_2_iso_code
        , subdivision_2_name
        , city_name
        , metro_code
        , time_zone
        , is_in_european_union
        }

instance ToNamedRecordOrdered GeoIPLocation where
  toNamedRecordOrdered
    GeoIPLocation
      { geoname_id
      , locale_code
      , continent_code
      , continent_name
      , country_iso_code
      , country_name
      , subdivision_1_iso_code
      , subdivision_1_name
      , subdivision_2_iso_code
      , subdivision_2_name
      , city_name
      , metro_code
      , time_zone
      , is_in_european_union
      } =
      empty
        |> ("geoname_id", toField geoname_id)
        |> ("locale_code", toField locale_code)
        |> ("continent_code", toField continent_code)
        |> ("continent_name", toField continent_name)
        |> ("country_iso_code", toField country_iso_code)
        |> ("country_name", toField country_name)
        |> ("subdivision_1_iso_code", toField subdivision_1_iso_code)
        |> ("subdivision_1_name", toField subdivision_1_name)
        |> ("subdivision_2_iso_code", toField subdivision_2_iso_code)
        |> ("subdivision_2_name", toField subdivision_2_name)
        |> ("city_name", toField city_name)
        |> ("metro_code", toField metro_code)
        |> ("time_zone", toField time_zone)
        |> ("is_in_european_union", toField is_in_european_union)

data WithIPBorders = WithIPBorders
  { lowerBorder :: Word64
  , upperBorder :: Word64
  , location :: GeoIPLocation
  }
  deriving (Generic, Show, Eq, Store)

instance FromRecord WithIPBorders where
  parseRecord v = do
    location <- parseRecord $ V.drop 2 v
    lowerBorder <- parseField $ v V.! 0
    upperBorder <- parseField $ v V.! 1
    pure $
      WithIPBorders
        { location
        , lowerBorder
        , upperBorder
        }

redisTSV :: CSVSettings
redisTSV =
  CSVSettings
    { csvSep = '|'
    , csvQuoteChar = Nothing
    }

instance ToNamedRecordOrdered WithIPBorders where
  toNamedRecordOrdered WithIPBorders{lowerBorder, upperBorder, location} =
    ("lowerBorder", toField lowerBorder)
      <| ("upperBorder", toField upperBorder)
      <| toNamedRecordOrdered location

instance FromNamedRecordOrdered WithIPBorders where
  parseNamedRecordOrdered omap =
    WithIPBorders
      <$> lookupOrdered omap "lowerBorder"
      <*> lookupOrdered omap "upperBorder"
      <*> parseNamedRecordOrdered omap

ipv4Lookup :: ByteString -> IPv4 -> Redis (Either String WithIPBorders)
ipv4Lookup bd_name IPv4{getIPv4 = w} = do
  res <- zrevrangebyscoreLimit bd_name (fromIntegral w) 1.0 0 1
  pure $ do
    bss <- first show res
    case bss of
      [] -> Left "No data"
      [a] -> do
            row <- parseRow redisTSV a >>= maybe (Left "Can't parse row") Right
            runParser (parseRecord @WithIPBorders (V.fromList row))
      _ -> Left "multiple answer"
    