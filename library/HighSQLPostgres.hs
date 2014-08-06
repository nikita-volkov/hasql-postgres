module HighSQLPostgres where

import HighSQLPostgres.Prelude hiding (Error)
import HighSQL.Backend
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parsers as Parsers
import qualified HighSQLPostgres.Renderers as Renderers
import qualified HighSQLPostgres.Connection as Connection
import qualified HighSQLPostgres.LibPQ.Connection as LibPQConnection


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
  type Connection Postgres =
    Connection.Connection
  connect p =
    withExceptT liftFailure $ Connection.establish settings
    where
      settings =
        LibPQConnection.Settings (host p) (port p) (user p) (password p) (database p)
      liftFailure =
        ConnectionFailure . fromString . show
  disconnect c =
    lift $ Connection.close c
  execute b vl =
    $notImplemented


-- * Values
-------------------------

-- |
-- Make a 'renderArgument' function with the 'Text' format.
{-# INLINE mkRenderArgument #-}
mkRenderArgument :: L.Oid -> Renderers.R a -> (a -> (L.Oid, ByteString, L.Format))
mkRenderArgument o r a =
  (o, Renderers.run a r, L.Text)

{-# INLINE mkParseResult #-}
mkParseResult :: Parsers.P a -> (Result Postgres -> Maybe a)
mkParseResult p (Result b) =
  either (const Nothing) Just $ Parsers.run b p 

instance Value Int Postgres where
  renderArgument = mkRenderArgument OID.int8 Renderers.int
  parseResult    = mkParseResult Parsers.integral

instance Value Word Postgres where
  renderArgument = mkRenderArgument OID.int8 Renderers.word
  parseResult    = mkParseResult Parsers.integral

instance Value Int64 Postgres where
  renderArgument = mkRenderArgument OID.int8 Renderers.int64
  parseResult    = mkParseResult Parsers.integral

instance Value TimeOfDay Postgres where
  renderArgument = mkRenderArgument OID.time Renderers.timeOfDay
  parseResult    = mkParseResult Parsers.timeOfDay


