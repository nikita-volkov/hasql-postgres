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
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.PTI as PTI
import qualified Hasql.Postgres.Mapping as Mapping
import qualified Hasql.Postgres.Session.Transaction as Transaction
import qualified Hasql.Postgres.Session.Execution as Execution
import qualified Language.Haskell.TH as TH
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Text.Encoding as Text
import qualified ListT


-- |
-- Settings of a Postgres backend.
data Postgres =
    PostgresParams {
      host :: ByteString,
      port :: Word16,
      user :: Text,
      password :: Text,
      database :: Text
    }
  | PostgresRaw ByteString

instance Backend.Backend Postgres where
  data StatementArgument Postgres = 
    StatementArgument PQ.Oid (Mapping.Environment -> Maybe ByteString)
  data Result Postgres = 
    Result Mapping.Environment (Maybe ByteString)
  data Connection Postgres = 
    Connection {
      connection :: !PQ.Connection,
      executionEnv :: !Execution.Env,
      transactionEnv :: !Transaction.Env,
      mappingEnv :: !Mapping.Environment
    }
  connect p =
    do
      r <- Connector.open settings
      c <- either (\e -> throwIO $ Backend.CantConnect $ fromString $ show e) return r
      ee <- Execution.newEnv c
      Connection <$> pure c <*> pure ee <*> Transaction.newEnv ee <*> getIntegerDatetimes c
    where
      settings =
        case p of
          (PostgresRaw r) -> Connector.SettingsRaw r
          params -> Connector.SettingsParams (host params) (port params) (user params) (password params) (database params)
      getIntegerDatetimes c =
        fmap parseResult $ PQ.parameterStatus c "integer_datetimes"
        where
          parseResult = 
            \case
              Just "on" -> True
              _ -> False
  disconnect c =
    PQ.finish (connection c)
  execute s = 
    do  s' <- liftStatement s
        liftExecution $ Execution.unitResult =<< Execution.statement s'
  executeAndGetMatrix s =
    do  s' <- liftStatement s
        c <- id
        (fmap . fmap . fmap . fmap) (Result (mappingEnv c)) $ liftExecution $ 
          Execution.vectorResult =<< Execution.statement s'
  executeAndStream s =
    do  s' <- liftStatement s
        c <- id
        return $ return $ liftTransactionStream (Transaction.streamWithCursor s') c
  executeAndCountEffects s =
    do  s' <- liftStatement s
        liftExecution $ Execution.countResult =<< Execution.statement s'
  beginTransaction (isolation, write) = 
    liftTransaction $ Transaction.beginTransaction (mapIsolation isolation, write)
    where
      mapIsolation =
        \case
          Backend.Serializable    -> Statement.Serializable
          Backend.RepeatableReads -> Statement.RepeatableRead
          Backend.ReadCommitted   -> Statement.ReadCommitted
          Backend.ReadUncommitted -> Statement.ReadCommitted
  finishTransaction commit =
    liftTransaction $ Transaction.finishTransaction commit


-- |
-- Just a convenience alias to a function on connection.
-- Useful since most of the 'Backend' API is made up of such functions.
type M a =
  Backend.Connection Postgres -> a

liftExecution :: Execution.M a -> M (IO a)
liftExecution m =
  \c -> Execution.run (executionEnv c) m >>= either (throwIO . mapExecutionError) return

liftTransaction :: Transaction.M a -> M (IO a)
liftTransaction m =
  \c -> Transaction.run (transactionEnv c) m >>= either (throwIO . mapError) return
  where
    mapError =
      \case
        Transaction.NotInTransaction -> Backend.NotInTransaction
        Transaction.ExecutionError e -> mapExecutionError e

liftStatement :: Backend.Statement Postgres -> M Statement.Statement
liftStatement (template, arguments, preparable) c =
  (,,) template (map liftArgument arguments) preparable
  where
    liftArgument (StatementArgument o f) = 
      (,) o ((,) <$> f (mappingEnv c) <*> pure PQ.Binary)

liftTransactionStream :: Transaction.Stream -> M (Backend.Stream Postgres)
liftTransactionStream s c =
  (fmap . fmap) (Result (mappingEnv c)) $ hoist (flip liftTransaction c) s

mapExecutionError :: Execution.Error -> Backend.Error
mapExecutionError =
  \case
    Execution.UnexpectedResult m     -> Backend.UnexpectedResult m
    Execution.ErroneousResult m      -> Backend.ErroneousResult m
    Execution.UnparsableTemplate t m -> Backend.UnparsableTemplate $
                                        "Message: " <> m <> "; " <>
                                        "Template: " <> fromString (show t)
    Execution.TransactionConflict    -> Backend.TransactionConflict


-- * Mappings
-------------------------
-- Not using TH to generate instances
-- to be able to document them.
-------------------------


{-# INLINE renderValueUsingMapping #-}
renderValueUsingMapping :: Mapping.Mapping a => a -> Backend.StatementArgument Postgres
renderValueUsingMapping x = 
  StatementArgument
    (PTI.oidPQ $ Mapping.oid x)
    (flip Mapping.encode x)

{-# INLINE parseResultUsingMapping #-}
parseResultUsingMapping :: Mapping.Mapping a => Backend.Result Postgres -> Either Text a
parseResultUsingMapping (Result e x) = 
  Mapping.decode e x

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
-- 'Maybe' cannot be used to wrap an intermediate level in a multidimensional list.
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
-- Maps to @timestamp@.
instance Backend.Mapping Postgres LocalTime where
  renderValue = renderValueUsingMapping
  parseResult = parseResultUsingMapping

-- |
-- Maps to @timestamptz@.
-- 
-- /NOTICE/
-- 
-- Postgres does not store the timezone information of @timestamptz@.
-- Instead it stores a UTC value and performs silent conversions
-- to the currently set timezone, when dealt with in the text format.
-- However this library bypasses the silent conversions
-- and communicates with Postgres using the UTC values directly.
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

