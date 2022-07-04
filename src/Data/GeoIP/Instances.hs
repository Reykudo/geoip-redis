{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Data.GeoIP.Instances where

import qualified Data.ByteString.Char8 as BS
import Data.CSV.Conduit.Conversion
import qualified Data.Text as Text
import Net.IPv4 as IPv4
import Net.IPv6 as IPv6

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
