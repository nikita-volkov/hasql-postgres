module HighSQLPostgres where

import HighSQLPostgres.Prelude hiding (Error)
import HighSQL.Backend
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parser as Parser
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.Session as Session
import qualified HighSQLPostgres.Statement as Statement
import qualified HighSQLPostgres.LibPQ.Connector as Connector
import qualified ListT


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
    (L.Oid, Maybe (ByteString, L.Format))
  newtype Result Postgres = 
    Result (Maybe ByteString)
  newtype Connection Postgres = 
    Connection Session.Context
  connect p =
    do
      r <- runExceptT $ Connector.open settings
      case r of
        Left e -> 
          throwIO $ CantConnect $ fromString $ show e
        Right c ->
          Connection <$> Session.newContext c
    where
      settings =
        Connector.Settings (host p) (port p) (user p) (password p) (database p)
  disconnect (Connection (c, _, _)) =
    L.finish c
  execute s (Connection c) =
    runSession c $
      Session.unitResult =<< Session.execute (mkSessionStatement s)
  executeAndStream s (Connection c) =
    runSession c $
      return . (hoistSessionStream c) =<< Session.streamResult =<< Session.execute (mkSessionStatement s)
  executeAndStreamWithCursor s (Connection c) =
    runSession c $
      return . (hoistSessionStream c) =<< Session.streamWithCursor (mkSessionStatement s)
  executeAndCountEffects s (Connection c) =
    do
      r <- runSession c $ Session.rowsAffectedResult =<< Session.execute (mkSessionStatement s)
      either 
        (throwIO . ResultParsingError (Just (r, typeOf (undefined :: Integer))) . Just) 
        return 
        (Parser.run r Parser.integral)
  inTransaction (isolation, write) io (Connection c) =
    runSession c $ Session.inTransaction (sessionIsolation, write) $ liftIO io
    where
      sessionIsolation =
        case isolation of
          Serializable    -> Statement.Serializable
          RepeatableReads -> Statement.RepeatableRead
          ReadCommitted   -> Statement.ReadCommitted
          ReadUncommitted -> Statement.ReadCommitted

runSession :: Session.Context -> Session.Session r -> IO r
runSession c s =
  Session.run c s >>= either onError return
  where
    onError =
      \case
        Session.NotInTransaction -> $bug "Unexpected NotInTransaction error"
        Session.UnexpectedResult -> throwIO $ ResultParsingError Nothing Nothing
        Session.ResultError e    -> $bug $ "Unexpected result error: " <> show e

hoistSessionStream :: Session.Context -> Session.Stream -> ResultsStream Postgres
hoistSessionStream c =
  fmap (ListT.traverse (return . Result) . hoist (runSession c))

mkSessionStatement :: Statement Postgres -> Statement.Statement
mkSessionStatement (template, values) =
  (template, values, True)


-- * Mappings
-------------------------

-- |
-- Make a 'renderValue' function with the 'Text' format.
{-# INLINE mkRenderValue #-}
mkRenderValue :: L.Oid -> Renderer.R a -> (a -> (L.Oid, Maybe (ByteString, L.Format)))
mkRenderValue o r a =
  (o, Just (Renderer.run a r, L.Text))

{-# INLINE mkParseResult #-}
mkParseResult :: Parser.P a -> (Result Postgres -> Maybe a)
mkParseResult p (Result r) =
  do
    r' <- r
    either (const Nothing) Just $ Parser.run r' p

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

