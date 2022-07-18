{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main  where

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
import Data.GeoIP.Location
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Int
import qualified Data.Map as Map
import Data.Maybe hiding (mapMaybe)
import Data.Store (PeekException, decode, encode)
import Data.Text (Text)
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
import Data.GeoIP.Instances ()

data Options = Options
  { redis :: ConnectInfo
  , blockDb :: ByteString
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
      )
    <*> strOption
      ( long "db-name"
          <> short 'd'
      )
    <*> strOption
      ( long "blocks-file"
          <> short 'b'
      )
    <*> strOption
      ( long "location-file"
          <> short 'l'
      )
    <*> flag False True
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
    , blockDb
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
                .| chunksOf 10000
                .| awaitForever
                  ( \(fmap getNamedOrdered -> records) -> do
                      let handle GeoIPBlock{network, geoname_id} =
                            ( fromIntegral @_ @Double . getIPv4 . lowerInclusive $ network
                            , BS8.pack . show $ geoname_id
                            )

                      liftIO $ do
                        modifyIORef count (+ length records)
                        runRedis redisConn $ do
                          let records1 = do
                                GeoIPBlock{geoname_id, network} <- records
                                let Just location = Map.lookup geoname_id lMap
                                    lowerBorder = fromIntegral $ getIPv4 . lowerInclusive $ network
                                    upperBorder = fromIntegral $ getIPv4 . upperInclusive $ network
                                pure (lowerBorder, WithIPBorders{location, lowerBorder, upperBorder})

                          zaddOpts
                            blockDb
                            (bimap fromIntegral (rowToStr redisTSV . NamedOrdered) <$> records1)
                            defaultZaddOpts{zaddCondition = Just Nx}
                  )
    Right c <- runRedis redisConn $ zcard blockDb
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

