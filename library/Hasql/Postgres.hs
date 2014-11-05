module Hasql.Postgres 
(
  Postgres(..), 
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
import qualified Hasql.Postgres.OIDMapping as OIDMapping
import qualified Data.Text.Encoding as Text
import qualified ListT
import qualified Language.Haskell.TH as TH


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

-- ** Helpers
-------------------------

-- |
-- Make a 'renderValue' function.
{-# INLINE mkRenderValue #-}
mkRenderValue :: PQ.Format -> PQ.Oid -> Renderer.R a -> (a -> Backend.StatementArgument Postgres)
mkRenderValue f o r a =
  StatementArgument (o, Just (Renderer.run a r, f))

{-# INLINE mkParseResult #-}
mkParseResult :: Parser.P a -> (Backend.Result Postgres -> Either Text a)
mkParseResult p (Result r) =
  do
    r' <- maybe (Left "Null result") Right r
    left (\t -> t <> "; Input: " <> (fromString . show) r') $ 
      Parser.run r' p

-- ** Instances
-------------------------

-- | Maps to the same type as the underlying value, 
-- encoding the 'Nothing' as /NULL/.
instance Backend.Mapping Postgres a => Backend.Mapping Postgres (Maybe a) where
  renderValue =
    \case
      Nothing -> 
        case Backend.renderValue ($bottom :: a) of
          StatementArgument (oid, _) -> StatementArgument (oid, Nothing)
      Just v ->
        Backend.renderValue v
  parseResult = 
    traverse (Backend.parseResult . Result . Just) . unpackResult

instance (Backend.Mapping Postgres a, Renderer.Renderable a, Parser.Parsable a, OIDMapping.OIDMapping a) => 
         Backend.Mapping Postgres (Vector a) where
  renderValue = 
    mkRenderValue PQ.Text (OIDMapping.identifyOID (undefined :: Vector a)) (Renderer.renderer Nothing)
  parseResult = 
    mkParseResult (Parser.parser Nothing)

let
  types =
    [ ''Bool,
      ''Int,
      ''Int8,
      ''Int16,
      ''Int32,
      ''Int64,
      ''Word,
      ''Word8,
      ''Word16,
      ''Word32,
      ''Word64,
      ''Float,
      ''Double,
      ''Scientific,
      ''Day,
      ''TimeOfDay,
      ''LocalTime,
      ''ZonedTime,
      ''UTCTime,
      ''Char,
      ''Text,
      ''LazyText,
      ''ByteString,
      ''LazyByteString ]
  in
    fmap concat $ forM types $ \t ->
      [d|
        instance Backend.Mapping Postgres $(TH.conT t) where
          renderValue = 
            mkRenderValue 
              PQ.Text 
              (OIDMapping.identifyOID (undefined :: $(TH.conT t)))
              (Renderer.renderer Nothing)
          parseResult =
            mkParseResult (Parser.parser Nothing)
      |]
