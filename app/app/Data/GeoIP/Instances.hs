{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.GeoIP.Instances where

import qualified Data.ByteString.Char8 as BS
import Data.CSV.Conduit.Conversion
import qualified Data.Text as Text
import Net.IPv4
import qualified Net.IPv4 as IPv4
import Net.IPv6
import qualified Net.IPv6 as IPv6
import Data.GeoIP.Block
import Data.GeoIP.Location
import qualified Data.Vector as V
import Data.CSV.Conduit
import Data.ByteString hiding (empty)
import Data.Word
import Database.Redis
import Data.CSV.Conduit.Parser.ByteString
import Data.Bifunctor
import Data.Map.Ordered

instance FromField IPv4Range where
  parseField bs = do
    Just v <- pure $ IPv4.decodeRange $ Text.pack . BS.unpack $ bs
    pure v

instance ToField IPv4Range where
  toField = BS.pack . Text.unpack . IPv4.encodeRange

instance FromField IPv6Range where
  parseField bs = do
    Just v <- pure $ IPv6.decodeRange $ Text.pack . BS.unpack $ bs
    pure v

instance ToField IPv6Range where
  toField = BS.pack . Text.unpack . IPv6.encodeRange

instance FromField Bool where
  parseField = \case
    "0" -> pure False
    "1" -> pure True
    _ -> fail "Could not parse Bool"

instance ToField Bool where
  toField = \case
    False -> "0"
    True -> "1"


instance (FromField r) => FromNamedRecordOrdered (GeoIPBlock r) where
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

instance (ToField r) => ToNamedRecordOrdered (GeoIPBlock r) where
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

deriving instance FromRecord GeoIPLocation
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

ipv4Lookup :: ByteString -> Word64 -> Redis (Either String WithIPBorders)
ipv4Lookup bd_name w = do
  res <- zrevrangebyscoreLimit bd_name (fromIntegral w) 1.0 0 1
  pure $ do
    bss <- first show res
    case bss of
      [] -> Left "No data"
      [a] -> do
            row <- parseRow redisTSV a >>= maybe (Left "Can't parse row") Right
            runParser (parseRecord @WithIPBorders (V.fromList row))
      _ -> Left "multiple answer"
    