module HighSQLPostgres where

import HighSQLPostgres.Prelude hiding (Error)
import HighSQL.Backend
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parser as Parser
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.Connection as Connection
import qualified HighSQLPostgres.LibPQ.Connector as LibPQ.Connector


data Postgres =
  Postgres {
    host :: ByteString,
    port :: Word16,
    user :: Text,
    password :: Text,
    database :: Text
  }

instance Backend Postgres where
  type StatementArgument Postgres =
    (L.Oid, ByteString, L.Format)
  newtype Result Postgres =
    Result ByteString
  newtype Connection Postgres =
    Connection Connection.Connection
  connect p =
    either (throwIO . ConnectionLost . fromString . show) (return . Connection) =<< do
      runExceptT $ Connection.establish settings
    where
      settings =
        LibPQ.Connector.Settings (host p) (port p) (user p) (password p) (database p)
  disconnect (Connection c) =
    Connection.close c
  execute s c =
    $notImplemented
  executeAndStream s c =
    $notImplemented


-- * Mappings
-------------------------

-- |
-- Make a 'renderValue' function with the 'Text' format.
{-# INLINE mkRenderValue #-}
mkRenderValue :: L.Oid -> Renderer.R a -> (a -> (L.Oid, ByteString, L.Format))
mkRenderValue o r a =
  (o, Renderer.run a r, L.Text)

{-# INLINE mkParseResult #-}
mkParseResult :: Parser.P a -> (Result Postgres -> Maybe a)
mkParseResult p (Result b) =
  either (const Nothing) Just $ Parser.run b p 

instance Mapping Postgres Int where
  renderValue = mkRenderValue OID.int8 Renderer.int
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Word where
  renderValue = mkRenderValue OID.int8 Renderer.word
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Int64 where
  renderValue = mkRenderValue OID.int8 Renderer.int64
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres TimeOfDay where
  renderValue = mkRenderValue OID.time Renderer.timeOfDay
  parseResult = mkParseResult Parser.timeOfDay


