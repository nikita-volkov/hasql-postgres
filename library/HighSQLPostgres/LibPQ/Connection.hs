-- |
-- Mid-level abstractions over gritty details of \"lib-pq\".
module HighSQLPostgres.LibPQ.Connection where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified HighSQLPostgres.Renderers as Renderers


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


data Failure =
  BadStatus (Maybe ByteString) |
  UnsupportedVersion Int
  deriving (Show, Typeable)


-- |
-- Establish and initialize a connection.
new :: Settings -> ExceptT Failure IO L.Connection
new s =
  do
    c <- lift $ L.connectdb (Renderers.run s settingsRenderer)
    do
      s <- lift $ L.status c
      when (s /= L.ConnectionOk) $ 
        do
          m <- lift $ L.errorMessage c
          throwError $ BadStatus m
    do
      v <- lift $ L.serverVersion c
      when (v < 80200) $ throwError $ UnsupportedVersion v
    lift $ L.exec c "SET standard_conforming_strings TO on;SET datestyle TO ISO"
    return c


settingsRenderer :: Renderers.R Settings
settingsRenderer s =
  mconcat $ intersperse " " args
  where
    args =
      [
        "host="     <> Renderers.byteString (host s),
        "port="     <> Renderers.word16 (port s),
        "user="     <> Renderers.text (user s),
        "password=" <> Renderers.text (password s),
        "dbname="   <> Renderers.text (database s)
      ]


close :: L.Connection -> IO ()
close c =
  L.finish c


