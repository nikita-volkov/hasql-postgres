module Hasql.Postgres.Connector where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as PQ
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
open :: Settings -> EitherT Error IO PQ.Connection
open s =
  do
    c <- lift $ PQ.connectdb (settingsBS s)
    do
      s <- lift $ PQ.status c
      when (s /= PQ.ConnectionOk) $ 
        do
          m <- lift $ PQ.errorMessage c
          left $ BadStatus m
    do
      v <- lift $ PQ.serverVersion c
      when (v < 80200) $ left $ UnsupportedVersion v
    lift $ PQ.exec c $ mconcat $ map (<> ";") $ 
      [ 
        "SET client_encoding = 'UTF8'",
        "SET client_min_messages TO WARNING"
      ]
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
