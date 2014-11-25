module Hasql.Postgres.Connector where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE


data Settings =
  Settings {
    host :: ByteString,
    port :: Maybe Int,
    user :: Text,
    password :: Text,
    database :: Text
  }


-- |
-- Default settings.
settings :: Settings
settings = Settings mempty Nothing mempty mempty mempty


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
    B.intercalate " " $ filter (not . B.null) args
  where
    args =
       [
        "host="     <>? host s,
        "port="     <>? fromMaybe "" ((BC.pack . show) <$> port s),
        "user="     <>? TE.encodeUtf8 (user s),
        "password=" <>? TE.encodeUtf8 (password s),
        "dbname="   <>? TE.encodeUtf8 (database s)
      ]

(<>?) :: ByteString -> ByteString -> ByteString
a <>? b
  | B.null b  = mempty
  | otherwise = a <> b
