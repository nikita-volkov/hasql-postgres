module Hasql.Postgres.Session where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified Hasql.Postgres.ErrorCode as ErrorCode
import qualified Hasql.Postgres.OID as OID
import qualified Hasql.Postgres.Renderer as Renderer
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.TemplateConverter as TemplateConverter
import qualified Hasql.Postgres.LibPQ.Result as Result
import qualified Hasql.Postgres.LibPQ.StatementPreparer as StatementPreparer
import qualified ListT
import qualified Data.Vector as Vector


type Context =
  (L.Connection, StatementPreparer.StatementPreparer, IORef (Maybe Word))

newContext :: L.Connection -> IO Context
newContext c = 
  (,,) <$> pure c <*> StatementPreparer.new c <*> newIORef Nothing


-- * Errors
-------------------------

data Error =
  NotInTransaction |
  UnexpectedResult Text |
  ResultError Result.Error |
  UnparsableTemplate ByteString Text |
  TransactionConflict
  deriving (Show, Typeable)


-- * Session
-------------------------

type Session =
  ReaderT Context (ExceptT Error IO)

type Stream =
  ListT Session (Vector (Maybe ByteString))

-- |
-- Execute the session, throwing the exceptions.
{-# INLINE run #-}
run :: Context -> Session r -> IO (Either Error r)
run c s =
  runExceptT $ runReaderT s c

parseResult :: Maybe L.Result -> Session Result.Success
parseResult r =
  ReaderT $ \(c, _, _) -> lift (Result.parse c r) >>= either handler return
  where
    handler =
      \case
        Result.ResultError _ c _ _ _ | elem c codes -> throwError TransactionConflict
        e -> throwError $ ResultError e
      where
        codes =
          [
            ErrorCode.transaction_rollback,
            ErrorCode.transaction_integrity_constraint_violation,
            ErrorCode.serialization_failure,
            ErrorCode.statement_completion_unknown,
            ErrorCode.deadlock_detected
          ]

execute :: Statement.Statement -> Session Result.Success
execute s =
  parseResult =<< do
    (connection, preparer, _) <- ask
    let (template, params, preparable) = s
    convertedTemplate <-
      either (throwError . UnparsableTemplate template) return $ TemplateConverter.convert template
    case preparable of
      True -> do
        let (tl, vl) = unzip params
        key <- lift $ withExceptT ResultError $ StatementPreparer.prepare convertedTemplate tl preparer
        liftIO $ L.execPrepared connection key vl L.Text
      False -> do
        let params' = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) params
        liftIO $ L.execParams connection convertedTemplate params' L.Text

-- |
-- Requires to be in transaction.
nextName :: Session ByteString
nextName =
  do
    (_, _, transactionStateRef) <- ask
    transactionState <- liftIO $ readIORef transactionStateRef
    nameCounter <- maybe (throwError NotInTransaction) return transactionState
    liftIO $ writeIORef transactionStateRef (Just $ succ nameCounter)
    return $ Renderer.run nameCounter $ \n -> Renderer.char 'v' <> Renderer.word n

{-# INLINE unitResult #-}
unitResult :: Result.Success -> Session ()
unitResult =
  \case
    Result.CommandOK _ -> return ()
    _ -> throwError $ UnexpectedResult "Not a unit"

{-# INLINE matrixResult #-}
matrixResult :: Result.Success -> Session Result.Matrix
matrixResult =
  \case
    Result.Matrix a -> return a
    _ -> throwError $ UnexpectedResult "Not a matrix"

{-# INLINE rowsAffectedResult #-}
rowsAffectedResult :: Result.Success -> Session ByteString
rowsAffectedResult =
  \case
    Result.CommandOK (Just n) -> return n
    _ -> throwError $ UnexpectedResult "Not an affected rows number"

-- |
-- Returns the cursor identifier.
declareCursor :: Statement.Statement -> Session Statement.Cursor
declareCursor statement =
  do
    name <- nextName
    unitResult =<< execute (Statement.declareCursor name statement)
    return name

closeCursor :: Statement.Cursor -> Session ()
closeCursor cursor =
  unitResult =<< execute (Statement.closeCursor cursor)

fetchFromCursor :: Statement.Cursor -> Session Result.Matrix
fetchFromCursor cursor =
  matrixResult =<< execute (Statement.fetchFromCursor cursor)

beginTransaction :: Statement.TransactionMode -> Session ()
beginTransaction mode =
  do
    (_, _, transactionStateRef) <- ask
    liftIO $ writeIORef transactionStateRef (Just 0)
    unitResult =<< execute (Statement.beginTransaction mode)

finishTransaction :: Bool -> Session ()
finishTransaction commit =
  do
    unitResult =<< execute (bool Statement.abortTransaction Statement.commitTransaction commit)
    (_, _, transactionStateRef) <- ask
    liftIO $ writeIORef transactionStateRef Nothing

streamWithCursor :: Statement.Statement -> Session Stream
streamWithCursor statement =
  do
    cursor <- declareCursor statement
    return $ 
      let loop = do
            chunk <- lift $ fetchFromCursor cursor
            guard $ not $ Vector.null chunk
            Vector.foldl step mempty chunk <> loop
          step z r = z <> pure r
          in loop


