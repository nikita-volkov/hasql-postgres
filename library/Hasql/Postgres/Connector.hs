module Hasql.Postgres.Connector where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
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
  mconcat $ intersperse (BB.char7 ' ') args
  where
    args =
      [
        BB.string7 "host="     <> BB.byteString (host s),
        BB.string7 "port="     <> BB.word16Dec (port s),
        BB.string7 "user="     <> BB.byteString (TE.encodeUtf8 (user s)),
        BB.string7 "password=" <> BB.byteString (TE.encodeUtf8 (password s)),
        BB.string7 "dbname="   <> BB.byteString (TE.encodeUtf8 (database s))
      ]
