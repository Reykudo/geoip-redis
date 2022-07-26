{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Conduit hiding (connect)
import Control.Exception
import Control.Monad.Reader.Class (ask)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

-- import Data.CSV.Conduit
-- import Data.CSV.Conduit.Conversion (NamedOrdered (..))

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion
import Data.Conduit.Combinators hiding (head, length, print)
import Data.Conduit.List (chunksOf, mapMaybe, sourceList, take)
import Data.Default
import Data.Foldable (Foldable (foldl'), for_, traverse_)
import Data.Function
import Data.Functor
import Data.GeoIP.Block
import Data.GeoIP.GeoIPFull
import Data.GeoIP.Instances (redisTSV)
import Data.GeoIP.Location
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Int
import qualified Data.Map as Map
import Data.Maybe hiding (mapMaybe)
import Data.Store (PeekException, decode, encode)
import qualified Data.Store as Store
import Data.Store.Streaming
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Database.Redis hiding (decode, info)
import Debug.Pretty.Simple
import GHC.Float (double2Int)
import Net.IPv4 hiding (decode, encode, print)
import Options.Applicative
import Text.Pretty.Simple
import Unsafe.Coerce
import Prelude hiding (mapM, mapM_, take)

data Options = Options
  { redis :: ConnectInfo
  , sortedSetName :: ByteString
  , blocksFile :: FilePath
  , locationFile :: FilePath
  , ipv6 :: Bool
  }
connInfoReadM :: ReadM ConnectInfo
connInfoReadM = eitherReader parseConnectInfo

options =
  Options
    <$> option
      connInfoReadM
      ( long "redis-conn"
          <> short 'r'
          <> help "Redis connection string like \"redis://user@localhost:6379/1\""
      )
    <*> strOption
      ( long "sorted-set"
          <> short 'd'
          <> help "Name for sorted set, where store data"
          <> value "geoip_ipv4"
      )
    <*> strOption
      ( long "blocks-file"
          <> short 'b'
          <> help "Path to block data, like \"./GeoLite2-City-Blocks-IPv4.csv'\""
      )
    <*> strOption
      ( long "location-file"
          <> short 'l'
          <> help "Path to locations data, like \"./GeoLite2-City-Locations-ru.csv\""
      )
    <*> flag
      False
      True
      ( long "ipv6"
          <> short '6'
          -- <> opt
      )

main :: IO ()
main = do
  Options
    { redis
    , blocksFile
    , locationFile
    , sortedSetName
    , ipv6
    } <-
    execParser $
      options
        `info` ( fullDesc
                  <> progDesc "Load GeoIP data into redis"
                  -- <> header "hello - a test for optparse-applicative"
               )

  locationMap <- newIORef $ [] @(Int64, GeoIPLocation)
  count <- newIORef 0
  withConnect redis $ \redisConn -> do
    -- concurrently_
    runConduitRes $ do
      sourceFile locationFile
        .| intoCSV @_ @(NamedOrdered GeoIPLocation) def
        .| chunksOf 1000
        .| awaitForever
          ( \(fmap getNamedOrdered -> records) -> do
              liftIO $ atomicModifyIORef @_ @() locationMap (\a -> (foldl' (\acc l@GeoIPLocation{geoname_id} -> (geoname_id, l) : acc) a records, ()))
          )
      curLocs <- liftIO $ readIORef locationMap
      let lMap = Map.fromDistinctDescList curLocs
      if ipv6
        then do pure ()
        else do
          sourceFile blocksFile
            .| intoCSV @_ @(NamedOrdered (GeoIPBlock IPv4Range)) def
            .| mapM
              ( \( getNamedOrdered ->
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
                  ) -> do
                    let Just
                          ( GeoIPLocation
                              { locale_code
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
                            ) = Map.lookup geoname_id lMap
                        lowerBorder = fromIntegral . getIPv4 . lowerInclusive $ network
                        upperBorder = fromIntegral . getIPv4 . upperInclusive $ network
                    pure
                      GeoIPFull
                        { lowerBorder
                        , upperBorder
                        , latitude
                        , longitude
                        , accuracy_radius
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
                        , postal_code
                        , metro_code
                        , time_zone
                        , is_in_european_union
                        }
              )
            .| chunksOf 1000
            -- .| conduitEncode
            .| mapM_
              ( \(wibs) -> do
                  let records1 = [(fromIntegral lowerBorder :: Double, Store.encode $ fmap (Text.encodeUtf8) w) | w@GeoIPFull{lowerBorder} <- wibs]
                  liftIO . runRedis redisConn $
                    zaddOpts
                      sortedSetName
                      (records1)
                      defaultZaddOpts{zaddCondition = Just Nx}
                  pure ()
              )
    Right c <- runRedis redisConn $ zcard sortedSetName
    c1 <- readIORef count
    pPrint (c, c1)
    pure ()

-- deduceLoc :: IPv4 -> IO ()
-- deduceLoc (IPv4 w) = do
--   let Right c = parseConnectInfo $ "redis://user@localhost:6379/0"
--   con <- connect c

--   a <- runRedis con $ ipv4Lookup "geoip_b" (fromOctets 109 252 75 100)
--   pPrint a

--   pure ()

-- pPrint $ decode @GeoIPLocation v

-- pure $ decode v

-- ipvRange
