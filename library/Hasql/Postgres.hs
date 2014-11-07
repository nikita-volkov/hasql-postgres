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
import qualified Hasql.Postgres.PTI as PTI
import qualified Hasql.Postgres.Mapping as Mapping
import qualified Language.Haskell.TH as TH
import qualified Data.Attoparsec.ByteString.Char8 as Atto
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
    {-# SCC "executeAndGetMatrix" #-} 
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
          return $ fromString $ 'v' : show counter
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
      case Atto.parseOnly (Atto.decimal <* Atto.endOfInput) b of
        Left m -> 
          throwIO $ Backend.UnexpectedResult (fromString m)
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
        PQ.execPrepared (connection c) key vl PQ.Binary
      False -> do
        let params' = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) params
        PQ.execParams (connection c) convertedTemplate params' PQ.Binary

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

let
  types =
    [ 
      [t|Maybe|],
      [t|[]|],
      [t|Vector|]
    ]
  in
    fmap concat $ forM types $ \t ->
      [d|
        instance Mapping.Mapping ($t a) => Backend.Mapping Postgres ($t a) where
          renderValue x = 
            StatementArgument (oid, (,) <$> value <*> pure PQ.Binary)
            where
              oid = PTI.oidPQ $ Mapping.oid x
              value = Mapping.encode x
          parseResult (Result x) = 
            Mapping.decode x

      |]

let
  types =
    [ 
      [t|Int|],
      [t|Int8|],
      [t|Int16|],
      [t|Int32|],
      [t|Int64|],
      [t|Word|],
      [t|Word8|],
      [t|Word16|],
      [t|Word32|],
      [t|Word64|],
      [t|Float|],
      [t|Double|],
      [t|Scientific|],
      [t|Day|],
      [t|TimeOfDay|],
      [t|(TimeOfDay, TimeZone)|],
      [t|LocalTime|],
      [t|UTCTime|],
      [t|DiffTime|],
      [t|Char|],
      [t|Text|],
      [t|LazyText|],
      [t|ByteString|],
      [t|LazyByteString|],
      [t|Bool|],
      [t|UUID|]
    ]
  in
    fmap concat $ forM types $ \t ->
      [d|
        instance Backend.Mapping Postgres $t where
          renderValue x = 
            StatementArgument (oid, (,) <$> value <*> pure PQ.Binary)
            where
              oid = PTI.oidPQ $ Mapping.oid x
              value = Mapping.encode x
          parseResult (Result x) = 
            Mapping.decode x

      |]
