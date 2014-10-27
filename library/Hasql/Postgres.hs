module Hasql.Postgres 
(
  -- * Settings for Hasql Initialization
  Postgres(..), 
  -- * Supported Mappings
  Backend.Mapping,
)
where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Backend as Backend
import qualified Hasql.Postgres.Connector as Connector
import qualified Hasql.Postgres.ResultParser as ResultParser
import qualified Hasql.Postgres.ResultHandler as ResultHandler
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.StatementPreparer as StatementPreparer
import qualified Hasql.Postgres.TemplateConverter as TemplateConverter
import qualified Hasql.Postgres.Parser as Parser
import qualified Hasql.Postgres.Renderer as Renderer
import qualified Hasql.Postgres.OID as OID
import qualified Data.Text.Encoding as Text
import qualified ListT


-- |
-- Settings of a Postgres backend.
data Postgres =
  Postgres {
    host :: ByteString,
    port :: Word16,
    user :: Text,
    password :: Text,
    database :: Text
  }

instance Backend.Backend Postgres where
  newtype StatementArgument Postgres = 
    StatementArgument {unpackStatementArgument :: (PQ.Oid, Maybe (ByteString, PQ.Format))}
  newtype Result Postgres = 
    Result {unpackResult :: (Maybe ByteString)}
  data Connection Postgres = 
    Connection {
      connection :: !PQ.Connection, 
      preparer :: !StatementPreparer.StatementPreparer,
      transactionState :: !(IORef (Maybe Word))
    }
  connect p =
    do
      r <- runExceptT $ Connector.open settings
      case r of
        Left e -> 
          throwIO $ Backend.CantConnect $ fromString $ show e
        Right c ->
          Connection <$> pure c <*> StatementPreparer.new c <*> newIORef Nothing
    where
      settings =
        Connector.Settings (host p) (port p) (user p) (password p) (database p)
  disconnect c =
    PQ.finish (connection c)
  execute s c = 
    ResultHandler.unit =<< execute (liftStatement s) c
  executeAndGetMatrix s c =
    unsafeCoerce . ResultHandler.rowsVector =<< execute (liftStatement s) c
  executeAndStream s c =
    do
      name <- declareCursor
      return $ unsafeCoerce $
        let loop = do
              chunk <- lift $ fetchFromCursor name
              null <- lift $ ListT.null chunk
              guard $ not null
              chunk <> loop
            in loop
    where
      nextName = 
        do
          counterM <- readIORef (transactionState c)
          counter <- maybe (throwIO Backend.NotInTransaction) return counterM
          writeIORef (transactionState c) (Just (succ counter))
          return $ Renderer.run counter $ \n -> Renderer.char 'v' <> Renderer.word n
      declareCursor =
        do
          name <- nextName
          ResultHandler.unit =<< execute (Statement.declareCursor name (liftStatement s)) c
          return name
      fetchFromCursor name =
        ResultHandler.rowsStream =<< execute (Statement.fetchFromCursor name) c
      closeCursor name =
        ResultHandler.unit =<< execute (Statement.closeCursor name) c
  executeAndCountEffects s c =
    do
      b <- ResultHandler.rowsAffected =<< execute (liftStatement s) c
      case Parser.run b Parser.unsignedIntegral of
        Left m -> 
          throwIO $ Backend.UnexpectedResult m
        Right r ->
          return r
  beginTransaction (isolation, write) c = 
    do
      writeIORef (transactionState c) (Just 0)
      ResultHandler.unit =<< execute (Statement.beginTransaction (statementIsolation, write)) c
    where
      statementIsolation =
        case isolation of
          Backend.Serializable    -> Statement.Serializable
          Backend.RepeatableReads -> Statement.RepeatableRead
          Backend.ReadCommitted   -> Statement.ReadCommitted
          Backend.ReadUncommitted -> Statement.ReadCommitted
  finishTransaction commit c =
    do
      ResultHandler.unit =<< execute (bool Statement.abortTransaction Statement.commitTransaction commit) c
      writeIORef (transactionState c) Nothing

liftStatement :: Backend.Statement Postgres -> Statement.Statement
liftStatement (template, values) =
  (template, map unpackStatementArgument values, True)

execute :: Statement.Statement -> Backend.Connection Postgres -> IO ResultParser.Result
execute s c =
  ResultParser.parse (connection c) =<< do
    let (template, params, preparable) = s
    convertedTemplate <- convertTemplate template
    case preparable of
      True -> do
        let (tl, vl) = unzip params
        key <- StatementPreparer.prepare convertedTemplate tl (preparer c)
        PQ.execPrepared (connection c) key vl PQ.Text
      False -> do
        let params' = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) params
        PQ.execParams (connection c) convertedTemplate params' PQ.Text

convertTemplate :: ByteString -> IO ByteString
convertTemplate t =
  case TemplateConverter.convert t of
    Left m -> 
      throwIO $ Backend.UnparsableTemplate $ 
        "Template: " <> Text.decodeLatin1 t <> ". " <>
        "Error: " <> m <> "."
    Right r ->
      return r



-- * Mappings
-------------------------

-- | Maps to the same type as the underlying value, 
-- encoding the 'Nothing' as /NULL/.
instance Backend.Mapping Postgres a => Backend.Mapping Postgres (Maybe a) where
  renderValue =
    \case
      Nothing -> 
        case Backend.renderValue (undefined :: a) of
          StatementArgument (oid, _) -> StatementArgument (oid, Nothing)
      Just v ->
        Backend.renderValue v
  parseResult = traverse (Backend.parseResult . Result . Just) . unpackResult

-- | Maps to \"bool\".
instance Backend.Mapping Postgres Bool where
  renderValue = mkRenderValue OID.bool Renderer.bool
  parseResult = mkParseResult Parser.bool

-- | Maps to \"int8\".
instance Backend.Mapping Postgres Int where
  renderValue = mkRenderValue OID.int8 Renderer.int
  parseResult = mkParseResult Parser.integral

-- | Maps to \"int2\".
instance Backend.Mapping Postgres Int8 where
  renderValue = mkRenderValue OID.int2 Renderer.int8
  parseResult = mkParseResult Parser.integral

-- | Maps to \"int2\".
instance Backend.Mapping Postgres Int16 where
  renderValue = mkRenderValue OID.int2 Renderer.int16
  parseResult = mkParseResult Parser.integral

-- | Maps to \"int4\".
instance Backend.Mapping Postgres Int32 where
  renderValue = mkRenderValue OID.int4 Renderer.int32
  parseResult = mkParseResult Parser.integral

-- | Maps to \"int8\".
instance Backend.Mapping Postgres Int64 where
  renderValue = mkRenderValue OID.int8 Renderer.int64
  parseResult = mkParseResult Parser.integral

-- | Maps to \"int8\".
instance Backend.Mapping Postgres Word where
  renderValue = mkRenderValue OID.int8 Renderer.word
  parseResult = mkParseResult Parser.unsignedIntegral

-- | Maps to \"int2\".
instance Backend.Mapping Postgres Word8 where
  renderValue = mkRenderValue OID.int2 Renderer.word8
  parseResult = mkParseResult Parser.unsignedIntegral

-- | Maps to \"int4\".
instance Backend.Mapping Postgres Word16 where
  renderValue = mkRenderValue OID.int4 Renderer.word16
  parseResult = mkParseResult Parser.unsignedIntegral

-- | Maps to \"int8\".
instance Backend.Mapping Postgres Word32 where
  renderValue = mkRenderValue OID.int8 Renderer.word32
  parseResult = mkParseResult Parser.unsignedIntegral

-- | Maps to \"int8\".
instance Backend.Mapping Postgres Word64 where
  renderValue = mkRenderValue OID.int8 Renderer.word64
  parseResult = mkParseResult Parser.unsignedIntegral

-- | Maps to \"float4\".
instance Backend.Mapping Postgres Float where
  renderValue = mkRenderValue OID.float4 Renderer.float
  parseResult = mkParseResult Parser.float

-- | Maps to \"float8\".
instance Backend.Mapping Postgres Double where
  renderValue = mkRenderValue OID.float8 Renderer.double
  parseResult = mkParseResult Parser.double

-- | Maps to \"numeric\".
instance Backend.Mapping Postgres Scientific where
  renderValue = mkRenderValue OID.numeric Renderer.scientific
  parseResult = mkParseResult Parser.scientific

-- | Maps to \"date\".
instance Backend.Mapping Postgres Day where
  renderValue = mkRenderValue OID.date Renderer.day
  parseResult = mkParseResult Parser.day

-- | Maps to \"time\".
instance Backend.Mapping Postgres TimeOfDay where
  renderValue = mkRenderValue OID.time Renderer.timeOfDay
  parseResult = mkParseResult Parser.timeOfDay

-- | Maps to \"timestamp\".
instance Backend.Mapping Postgres LocalTime where
  renderValue = mkRenderValue OID.timestamp Renderer.localTime
  parseResult = mkParseResult Parser.localTime

-- | Maps to \"timestamptz\".
instance Backend.Mapping Postgres ZonedTime where
  renderValue = mkRenderValue OID.timestamptz Renderer.zonedTime 
  parseResult = mkParseResult Parser.zonedTime 

-- | Maps to \"timestamp\".
instance Backend.Mapping Postgres UTCTime where
  renderValue = mkRenderValue OID.timestamp Renderer.utcTime
  parseResult = mkParseResult Parser.utcTime

-- | Maps to \"varchar\".
instance Backend.Mapping Postgres Char where
  renderValue = mkRenderValue OID.varchar Renderer.char
  parseResult = mkParseResult Parser.utf8Char

-- | Maps to \"text\".
instance Backend.Mapping Postgres Text where
  renderValue = mkRenderValue OID.text Renderer.text
  parseResult = mkParseResult Parser.utf8Text


-- |
-- Make a 'renderValue' function with the 'Text' format.
{-# INLINE mkRenderValue #-}
mkRenderValue :: PQ.Oid -> Renderer.R a -> (a -> Backend.StatementArgument Postgres)
mkRenderValue o r a =
  StatementArgument (o, Just (Renderer.run a r, PQ.Text))

{-# INLINE mkParseResult #-}
mkParseResult :: Parser.P a -> (Backend.Result Postgres -> Either Text a)
mkParseResult p (Result r) =
  do
    r' <- maybe (Left "Null result") Right r
    left (\t -> "Input: " <> Text.decodeLatin1 r' <> ". Error: " <> t) $ 
      Parser.run r' p
