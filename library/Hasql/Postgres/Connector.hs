-- |
-- Mid-level abstractions over gritty details of \"lib-pq\".
module Hasql.Postgres.Connector where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE


data Settings =
  Settings {
    host :: ByteString,
    port :: Word16,
    user :: Text,
    password :: Text,
    database :: Text
  }


-- |
-- Default settings.
settings :: Settings
settings =
  Settings "127.0.0.1" 5432 "postgres" "" ""


data Error =
  BadStatus (Maybe ByteString) |
  UnsupportedVersion Int
  deriving (Show, Typeable)


-- |
-- Establish and initialize a connection.
open :: Settings -> ExceptT Error IO L.Connection
open s =
  do
    c <- lift $ L.connectdb (settingsBS s)
    do
      s <- lift $ L.status c
      when (s /= L.ConnectionOk) $ 
        do
          m <- lift $ L.errorMessage c
          throwError $ BadStatus m
    do
      v <- lift $ L.serverVersion c
      when (v < 80200) $ throwError $ UnsupportedVersion v
    lift $ L.exec c $ mconcat $ map (<> ";") $ 
      [ "SET standard_conforming_strings TO on",
        "SET datestyle TO ISO",
        "SET client_encoding = 'UTF8'",
        "SET client_min_messages TO WARNING",
        "SET bytea_output = 'hex'" ]
    return c


settingsBS :: Settings -> ByteString
settingsBS s =
  BL.toStrict $ BB.toLazyByteString $ 
  mconcat $ intersperse " " args
  where
    args =
      [
        "host="     <> BB.byteString (host s),
        "port="     <> BB.word16Dec (port s),
        "user="     <> TE.encodeUtf8Builder (user s),
        "password=" <> TE.encodeUtf8Builder (password s),
        "dbname="   <> TE.encodeUtf8Builder (database s)
      ]
