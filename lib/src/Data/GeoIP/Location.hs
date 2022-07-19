{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

deriving instance Store (GeoIPLocation1 ByteString)

