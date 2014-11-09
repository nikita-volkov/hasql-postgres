-- |
-- This module contains everything required 
-- to use \"hasql\" with Postgres.
-- For information on how it should be used consult the \"hasql\" docs.
-- 
-- Please note that there is a few limitations inflicted by Postgres,
-- encoding which in the type system would seriously burden the API,
-- so it was decided to make it the user's responsibility 
-- to make sure that certain conditions are satisfied during the runtime.
-- Particularly this concerns the 'Backend.Mapping' instances of 
-- @Maybe@, @[]@ and @Vector@.
-- For details consult the docs on those instances.
-- 
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
-- Not using TH to generate instances
-- to be able to document them.
-------------------------


{-# INLINE renderValueUsingMapping #-}
renderValueUsingMapping :: Mapping.Mapping a => a -> Backend.StatementArgument Postgres
renderValueUsingMapping x = 
  StatementArgument (oid, (,) <$> value <*> pure PQ.Binary)
  where
    oid = PTI.oidPQ $ Mapping.oid x
    value = Mapping.encode x

{-# INLINE parseResultUsingMapping #-}
parseResultUsingMapping :: Mapping.Mapping a => Backend.Result Postgres -> Either Text a
parseResultUsingMapping (Result x) = 
  Mapping.decode x

-- | 
-- Maps to the same type as the underlying value, 
-- encoding 'Nothing' as /NULL/.
-- 
-- /LIMITATION/
-- 
-- Multilevel 'Maybe's are not supported.
-- E.g., a value @Just Nothing@ of type @(Maybe (Maybe a))@ 
-- will be encoded the same way as @Nothing@.
instance Mapping.Mapping a => Backend.Mapping Postgres (Maybe a) where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to Postgres arrays. 
-- 
-- /LIMITATION 1/
-- 
-- In multidimensional lists all rows of a dimension must have the same length.
-- 
-- E.g., the following is a corrupt value:
-- 
-- > [[1,2], [3]]
-- 
-- The following is a valid one:
-- 
-- > [[1,2], [3,4], [5,6]]
-- 
-- /LIMITATION 2/
-- 
-- 'Maybe' cannot be used to wrap an intermediate level in a multidimensional array.
-- 
-- E.g., the following is a corrupt type:
-- 
-- > [Maybe [a]]
-- 
-- However, both the first level list and the value are allowed to be wrapped in 'Maybe'.
-- So the following is a valid type:
-- 
-- > Maybe [[[Maybe a]]]
-- 
-- /NOTICE/
-- 
-- Also, please note that since 'String' is just an alias to @['Char']@,
-- it will be mapped to an array of characters. 
-- So if you want to map to a textual type use 'Text' instead.
-- 
instance (Mapping.Mapping a, Mapping.ArrayMapping a) => Backend.Mapping Postgres [a] where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to Postgres' arrays.
-- 
-- Same rules as for the list instance apply. 
-- Consult its docs for details.
instance (Mapping.Mapping a, Mapping.ArrayMapping a) => Backend.Mapping Postgres (Vector a) where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int8@.
instance Backend.Mapping Postgres Int where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int2@.
instance Backend.Mapping Postgres Int8 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int2@.
instance Backend.Mapping Postgres Int16 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int4@.
instance Backend.Mapping Postgres Int32 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int8@.
instance Backend.Mapping Postgres Int64 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int8@.
instance Backend.Mapping Postgres Word where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int2@.
instance Backend.Mapping Postgres Word8 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int2@.
instance Backend.Mapping Postgres Word16 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int4@.
instance Backend.Mapping Postgres Word32 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @int8@.
instance Backend.Mapping Postgres Word64 where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @float4@.
instance Backend.Mapping Postgres Float where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @float8@.
instance Backend.Mapping Postgres Double where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @numeric@.
instance Backend.Mapping Postgres Scientific where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @date@.
instance Backend.Mapping Postgres Day where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @time@.
instance Backend.Mapping Postgres TimeOfDay where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @timetz@.
-- 
-- Unlike with @timestamptz@, 
-- Postgres does store the timezone information for @timetz@.
-- However the \"time\" library does not contain any composite type,
-- that fits the task, so we use a pair of 'TimeOfDay' and 'TimeZone'
-- to represent a value on the Haskell's side.
instance Backend.Mapping Postgres (TimeOfDay, TimeZone) where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @timestamptz@.
-- 
-- /NOTICE/
-- 
-- Postgres does not store the timezone information of @timestamptz@.
-- Instead it stores a UTC value and silently interconverts 
-- the incoming and outgoing values into a local time 
-- of the client application according to client application's timezone.
-- 
-- This is a notoriously questionable design decision by the Postgres authors.
-- This is why it is instead recommended to use @timestamp@ and 'UTCTime',
-- while manually handling the timezone conversions on the application side.
instance Backend.Mapping Postgres LocalTime where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @timestamp@.
instance Backend.Mapping Postgres UTCTime where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @interval@.
instance Backend.Mapping Postgres DiffTime where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @char@.
-- Note that it supports UTF-8 values.
instance Backend.Mapping Postgres Char where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @text@.
instance Backend.Mapping Postgres Text where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @text@.
instance Backend.Mapping Postgres LazyText where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @bytea@.
instance Backend.Mapping Postgres ByteString where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @bytea@.
instance Backend.Mapping Postgres LazyByteString where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @bool@.
instance Backend.Mapping Postgres Bool where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @uuid@.
instance Backend.Mapping Postgres UUID where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

