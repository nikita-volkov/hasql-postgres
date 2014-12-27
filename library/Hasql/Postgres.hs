-- |
-- This module contains everything required 
-- to use \"hasql\" with Postgres.
-- For information on how it should be used consult the \"hasql\" docs.
-- 
-- Please note that there is a few limitations inflicted by Postgres,
-- encoding which in the type system would seriously burden the API,
-- so it was decided to make it the user's responsibility 
-- to make sure that certain conditions are satisfied during the runtime.
-- Particularly this concerns the 'Bknd.CxValue' instances of 
-- @Maybe@, @[]@ and @Vector@.
-- For details consult the docs on those instances.
-- 
module Hasql.Postgres 
(
  Postgres,
  Connector.Settings(..),
  CxError(..),
  TxError(..),
  Unknown(..),
)
where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Backend as Bknd
import qualified Hasql.Postgres.Connector as Connector
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.PTI as PTI
import qualified Hasql.Postgres.Mapping as Mapping
import qualified Hasql.Postgres.Session.Transaction as Transaction
import qualified Hasql.Postgres.Session.Execution as Execution
import qualified Hasql.Postgres.Session.ResultProcessing as ResultProcessing
import qualified Language.Haskell.TH as TH
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified ListT


-- |
-- A connection to PostgreSQL.
data Postgres = 
  Postgres {
    connection :: !PQ.Connection,
    executionEnv :: !Execution.Env,
    transactionEnv :: !Transaction.Env,
    mappingEnv :: !Mapping.Environment
  }

data CxError =
  -- | 
  -- Impossible to connect. 
  -- A clarification might be given in the attached byte string.
  CantConnect (Maybe ByteString) |
  -- | 
  -- Server is running an unsupported version of Postgres.
  -- The parameter is the version in such a format,
  -- where a value @80105@ identifies a version @8.1.5@.
  UnsupportedVersion Int
  deriving (Show, Eq)


instance Bknd.Cx Postgres where
  type CxSettings Postgres =
    Connector.Settings
  type CxError Postgres =
    CxError
  acquireCx settings =
    runEitherT $ do
      c <- EitherT $ fmap (mapLeft connectorErrorMapping) $ Connector.open settings
      lift $ do
        e <- Execution.newEnv c
        Postgres <$> pure c <*> pure e <*> Transaction.newEnv e <*> getIntegerDatetimes c
    where
      getIntegerDatetimes c =
        fmap decodeValue $ PQ.parameterStatus c "integer_datetimes"
        where
          decodeValue = 
            \case
              Just "on" -> True
              _ -> False
      connectorErrorMapping =
        \case
          Connector.BadStatus x -> CantConnect x
          Connector.UnsupportedVersion x -> UnsupportedVersion x
  releaseCx =
    PQ.finish . connection


-- * Transactions
-------------------------

data TxError =
  -- |
  -- Received no response from the database.
  NoResult !(Maybe ByteString) |
  -- | 
  -- An error reported by the DB. Code, message, details, hint.
  -- 
  -- * The SQLSTATE code for the error. The SQLSTATE code identifies the type of error that has occurred; it can be used by front-end applications to perform specific operations (such as error handling) in response to a particular database error. For a list of the possible SQLSTATE codes, see Appendix A. This field is not localizable, and is always present.
  -- * The primary human-readable error message (typically one line). Always present.
  -- * Detail: an optional secondary error message carrying more detail about the problem. Might run to multiple lines.
  -- * Hint: an optional suggestion what to do about the problem. This is intended to differ from detail in that it offers advice (potentially inappropriate) rather than hard facts. Might run to multiple lines.
  ErroneousResult !ByteString !ByteString !(Maybe ByteString) !(Maybe ByteString) |
  -- |
  -- The database returned an unexpected result.
  -- Indicates an improper statement or a schema mismatch.
  UnexpectedResult !Text |
  -- |
  -- An attempt to perform an action, 
  -- which requires a transaction context, without one.
  -- 
  -- Currently it's only raised when trying to stream
  -- without establishing a transaction.
  NotInTransaction
  deriving (Show, Eq)


instance Bknd.CxTx Postgres where
  type TxError Postgres =
    TxError
  runTx p mode =
    runEitherT . runMaybeT . flip runReaderT p . inTransaction mode . interpretTx 


type Interpreter a =
  ReaderT Postgres (MaybeT (EitherT TxError IO)) a

liftExecution :: Execution.M a -> Interpreter a
liftExecution m =
  do
    r <- ReaderT $ \p -> liftIO $ Execution.run (executionEnv p) m
    either throwResultProcessingError return r

liftTransaction :: Transaction.M a -> Interpreter a
liftTransaction m =
  do
    r <- ReaderT $ \p -> liftIO $ Transaction.run (transactionEnv p) m
    either throwTransactionError return r
  where
    throwTransactionError =
      \case
        Transaction.NotInTransaction -> lift $ lift $ left $ NotInTransaction
        Transaction.ResultProcessingError a -> throwResultProcessingError a

throwResultProcessingError :: ResultProcessing.Error -> Interpreter a
throwResultProcessingError =
  \case
    ResultProcessing.NoResult a -> lift $ lift $ left $ NoResult a
    ResultProcessing.ErroneousResult a b c d -> lift $ lift $ left $ ErroneousResult a b c d
    ResultProcessing.UnexpectedResult a -> lift $ lift $ left $ UnexpectedResult a
    ResultProcessing.TransactionConflict -> lift $ mzero

convertStatement :: Bknd.Stmt Postgres -> Interpreter Statement.Statement
convertStatement s =
  asks $ \p -> 
    let
      liftParam (StmtParam o f) = 
        (,) o ((,) <$> f (mappingEnv p) <*> pure PQ.Binary)
      in 
        Statement.Statement
          (Statement.UnicodeTemplate (Bknd.stmtTemplate s))
          (toList $ fmap liftParam $ Bknd.stmtParams s)
          (Bknd.stmtPreparable s)

interpretTx :: Bknd.Tx Postgres a -> Interpreter a
interpretTx =
  iterTM $ \case
    Bknd.UnitTx stmt next -> do
      stmt' <- convertStatement stmt
      liftExecution $ Execution.unitResult =<< Execution.statement stmt'
      next
    Bknd.CountTx stmt next -> do
      stmt' <- convertStatement stmt
      r <- liftExecution $ Execution.countResult =<< Execution.statement stmt'
      next $ r
    Bknd.MaybeTx stmt next -> do
      stmt' <- convertStatement stmt
      r <- liftExecution $ Execution.vectorResult =<< Execution.statement stmt'
      r' <- 
        asks $ \p ->
          (fmap . fmap) (ResultValue (mappingEnv p)) $
          fmap Vector.head $ mfilter (not . Vector.null) $ pure $ r
      next r'
    Bknd.VectorTx stmt next -> do
      stmt' <- convertStatement stmt
      r <- liftExecution $ Execution.vectorResult =<< Execution.statement stmt'
      r' <- 
        asks $ \p -> 
          (fmap . fmap) (ResultValue (mappingEnv p)) $ r
      next r'
    Bknd.StreamTx stmt next -> do
      stmt' <- convertStatement stmt
      r <- liftTransaction $ Transaction.streamWithCursor stmt'
      r' <- 
        asks $ \p -> 
          (fmap . fmap) (ResultValue (mappingEnv p)) $
          hoist (lift . flip runReaderT p . liftTransaction) $
          r
      next r'

inTransaction :: Bknd.TxMode -> Interpreter a -> Interpreter a
inTransaction mode m =
  do
    liftTransaction $ beginTransaction
    result <- ReaderT $ \p -> lift $ lift $ runEitherT $ runMaybeT $ flip runReaderT p $ m
    case result of
      Left e -> do
        liftTransaction $ finishTransaction False
        lift $ lift $ left $ e
      Right Nothing -> do
        liftTransaction $ finishTransaction False
        mzero
      Right (Just r) -> do
        liftTransaction $ finishTransaction True
        return r
  where
    (,) beginTransaction finishTransaction =
      case mode of
        Nothing -> 
          (,) (return ()) 
              (const (return ()))
        Just (isolation, Nothing) -> 
          (,) (Transaction.beginTransaction (convertIsolation isolation, False))
              (Transaction.finishTransaction)
        Just (isolation, Just commit) ->
          (,) (Transaction.beginTransaction (convertIsolation isolation, True))
              (\commit' -> Transaction.finishTransaction (commit && commit'))
      where
        convertIsolation =
          \case
            Bknd.Serializable    -> Statement.Serializable
            Bknd.RepeatableReads -> Statement.RepeatableRead
            Bknd.ReadCommitted   -> Statement.ReadCommitted
            Bknd.ReadUncommitted -> Statement.ReadCommitted


-- * Mappings
-------------------------
-- Not using TH to generate instances
-- to be able to document them.
-------------------------

data instance Bknd.ResultValue Postgres =
  ResultValue !Mapping.Environment !(Maybe ByteString)

data instance Bknd.StmtParam Postgres =
  StmtParam !PQ.Oid !(Mapping.Environment -> Maybe ByteString)


{-# INLINE encodeValueUsingMapping #-}
encodeValueUsingMapping :: Mapping.Mapping a => a -> Bknd.StmtParam Postgres
encodeValueUsingMapping x = 
  StmtParam
    (PTI.oidPQ $ Mapping.oid x)
    (flip Mapping.encode x)

{-# INLINE decodeValueUsingMapping #-}
decodeValueUsingMapping :: Mapping.Mapping a => Bknd.ResultValue Postgres -> Either Text a
decodeValueUsingMapping (ResultValue e x) = 
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
instance Mapping.Mapping a => Bknd.CxValue Postgres (Maybe a) where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

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
instance (Mapping.Mapping a, Mapping.ArrayMapping a) => Bknd.CxValue Postgres [a] where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to Postgres' arrays.
-- 
-- Same rules as for the list instance apply. 
-- Consult its docs for details.
instance (Mapping.Mapping a, Mapping.ArrayMapping a) => Bknd.CxValue Postgres (Vector a) where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int8@.
instance Bknd.CxValue Postgres Int where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int2@.
instance Bknd.CxValue Postgres Int8 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int2@.
instance Bknd.CxValue Postgres Int16 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int4@.
instance Bknd.CxValue Postgres Int32 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int8@.
instance Bknd.CxValue Postgres Int64 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int8@.
instance Bknd.CxValue Postgres Word where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int2@.
instance Bknd.CxValue Postgres Word8 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int2@.
instance Bknd.CxValue Postgres Word16 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int4@.
instance Bknd.CxValue Postgres Word32 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @int8@.
instance Bknd.CxValue Postgres Word64 where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @float4@.
instance Bknd.CxValue Postgres Float where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @float8@.
instance Bknd.CxValue Postgres Double where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @numeric@.
instance Bknd.CxValue Postgres Scientific where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @date@.
instance Bknd.CxValue Postgres Day where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @time@.
instance Bknd.CxValue Postgres TimeOfDay where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @timetz@.
-- 
-- Unlike with @timestamptz@, 
-- Postgres does store the timezone information for @timetz@.
-- However the \"time\" library does not contain any composite type,
-- that fits the task, so we use a pair of 'TimeOfDay' and 'TimeZone'
-- to represent a value on the Haskell's side.
instance Bknd.CxValue Postgres (TimeOfDay, TimeZone) where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @timestamp@.
instance Bknd.CxValue Postgres LocalTime where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

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
instance Bknd.CxValue Postgres UTCTime where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @interval@.
instance Bknd.CxValue Postgres DiffTime where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @char@.
-- Note that it supports UTF-8 values.
instance Bknd.CxValue Postgres Char where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @text@.
instance Bknd.CxValue Postgres Text where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @text@.
instance Bknd.CxValue Postgres LazyText where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @bytea@.
instance Bknd.CxValue Postgres ByteString where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @bytea@.
instance Bknd.CxValue Postgres LazyByteString where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @bool@.
instance Bknd.CxValue Postgres Bool where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping

-- |
-- Maps to @uuid@.
instance Bknd.CxValue Postgres UUID where
  encodeValue = encodeValueUsingMapping
  decodeValue = decodeValueUsingMapping


-- ** Custom types
-------------------------

-- |
-- A wrapper around a 'ByteString',
-- which identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving the choice of the type to Postgres.
-- The bytestring needs to be encoded according to the Postgres binary format
-- of the type it expects.
-- 
-- Essentially this is a low-level hook into the phases of encoding and decoding
-- of values with custom codecs.
-- <http://hackage.haskell.org/package/postgresql-binary The "postgresql-binary" library> 
-- is your toolchain when dealing with this type.
newtype Unknown = 
  Unknown ByteString

-- |
-- Maps to @unknown@.
instance Bknd.CxValue Postgres Unknown where
  encodeValue (Unknown x) = 
    StmtParam (PTI.oidPQ (PTI.ptiOID (PTI.unknown))) (const $ Just x)
  decodeValue (ResultValue _ x) = 
    maybe (Left "Decoding a NULL to Unknown") (Right . Unknown) x

