module Hasql.Postgres where

import Hasql.Postgres.Prelude hiding (Error)
import Hasql.Backend
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.Text.Encoding
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified Hasql.Postgres.OID as OID
import qualified Hasql.Postgres.Parser as Parser
import qualified Hasql.Postgres.Renderer as Renderer
import qualified Hasql.Postgres.Session as Session
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.LibPQ.Connector as Connector
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
  newtype StatementArgument Postgres = 
    StatementArgument {unpackStatementArgument :: (L.Oid, Maybe (ByteString, L.Format))}
  newtype Result Postgres = 
    Result {unpackResult :: (Maybe ByteString)}
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
        (throwIO . UnparsableResult (typeOf (undefined :: Integer)) r) 
        return 
        (Parser.run r Parser.integral)
  beginTransaction (isolation, write) (Connection c) =
    runSession c $ Session.beginTransaction (sessionIsolation, write)
    where
      sessionIsolation =
        case isolation of
          Serializable    -> Statement.Serializable
          RepeatableReads -> Statement.RepeatableRead
          ReadCommitted   -> Statement.ReadCommitted
          ReadUncommitted -> Statement.ReadCommitted
  finishTransaction commit (Connection c) =
    runSession c $ Session.finishTransaction commit


runSession :: Session.Context -> Session.Session r -> IO r
runSession c s =
  Session.run c s >>= either onError return
  where
    onError =
      \case
        Session.NotInTransaction -> 
          $bug "Unexpected NotInTransaction error"
        Session.UnexpectedResult t -> 
          throwIO $ UnexpectedResultStructure t
        Session.ResultError e -> 
          $bug $ "Unexpected result error: " <> show e
        Session.UnparsableTemplate b t -> 
          $bug $ 
            "Unexpected unparsable template error. " <>
            "Template: " <> show b <> ". " <>
            "Error: " <> show t <> "."
        Session.TransactionConflict ->
          throwIO $ TransactionConflict

hoistSessionStream :: Session.Context -> Session.Stream -> ResultsStream Postgres
hoistSessionStream c =
  fmap (ListT.traverse (return . Result) . hoist (runSession c))

mkSessionStatement :: Statement Postgres -> Statement.Statement
mkSessionStatement (template, values) =
  (template, map unpackStatementArgument values, True)


-- * Mappings
-------------------------

instance Mapping Postgres a => Mapping Postgres (Maybe a) where
  renderValue =
    \case
      Nothing -> 
        case renderValue (undefined :: a) of
          StatementArgument (oid, _) -> StatementArgument (oid, Nothing)
      Just v ->
        renderValue v
  parseResult = traverse (parseResult . Result . Just) . unpackResult

instance Mapping Postgres Bool where
  renderValue = mkRenderValue OID.bool Renderer.bool
  parseResult = mkParseResult Parser.bool

instance Mapping Postgres Char where
  renderValue = mkRenderValue OID.varchar Renderer.char
  parseResult = mkParseResult Parser.utf8Char

instance Mapping Postgres Text where
  renderValue = mkRenderValue OID.text Renderer.text
  parseResult = mkParseResult Parser.utf8Text

instance Mapping Postgres Int where
  renderValue = mkRenderValue OID.int8 Renderer.int
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Int8 where
  renderValue = mkRenderValue OID.int2 Renderer.int8
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Int16 where
  renderValue = mkRenderValue OID.int2 Renderer.int16
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Int32 where
  renderValue = mkRenderValue OID.int4 Renderer.int32
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Int64 where
  renderValue = mkRenderValue OID.int8 Renderer.int64
  parseResult = mkParseResult Parser.integral

instance Mapping Postgres Word where
  renderValue = mkRenderValue OID.int8 Renderer.word
  parseResult = mkParseResult Parser.unsignedIntegral

instance Mapping Postgres Word8 where
  renderValue = mkRenderValue OID.int2 Renderer.word8
  parseResult = mkParseResult Parser.unsignedIntegral

instance Mapping Postgres Word16 where
  renderValue = mkRenderValue OID.int4 Renderer.word16
  parseResult = mkParseResult Parser.unsignedIntegral

instance Mapping Postgres Word32 where
  renderValue = mkRenderValue OID.int8 Renderer.word32
  parseResult = mkParseResult Parser.unsignedIntegral

instance Mapping Postgres Word64 where
  renderValue = mkRenderValue OID.int8 Renderer.word64
  parseResult = mkParseResult Parser.unsignedIntegral

instance Mapping Postgres Day where
  renderValue = mkRenderValue OID.date Renderer.day
  parseResult = mkParseResult Parser.day

instance Mapping Postgres TimeOfDay where
  renderValue = mkRenderValue OID.time Renderer.timeOfDay
  parseResult = mkParseResult Parser.timeOfDay

instance Mapping Postgres LocalTime where
  renderValue = mkRenderValue OID.timestamp Renderer.localTime
  parseResult = mkParseResult Parser.localTime

instance Mapping Postgres ZonedTime where
  renderValue = mkRenderValue OID.timestamptz Renderer.zonedTime 
  parseResult = mkParseResult Parser.zonedTime 

instance Mapping Postgres UTCTime where
  renderValue = mkRenderValue OID.timestamp Renderer.utcTime
  parseResult = mkParseResult Parser.utcTime


-- |
-- Make a 'renderValue' function with the 'Text' format.
{-# INLINE mkRenderValue #-}
mkRenderValue :: L.Oid -> Renderer.R a -> (a -> StatementArgument Postgres)
mkRenderValue o r a =
  StatementArgument (o, Just (Renderer.run a r, L.Text))

{-# INLINE mkParseResult #-}
mkParseResult :: Parser.P a -> (Result Postgres -> Either Text a)
mkParseResult p (Result r) =
  do
    r' <- maybe (Left "Null result") Right r
    left (\t -> "Input: " <> Data.Text.Encoding.decodeLatin1 r' <> ". Error: " <> t) $ 
      Parser.run r' p
