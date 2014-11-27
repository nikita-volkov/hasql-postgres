module Hasql.Postgres.Session.Transaction where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.HashTable.IO as Hashtables
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as Vector
import qualified Hasql.Postgres.Session.Execution as Execution
import qualified Hasql.Postgres.Statement as Statement


-- * Environment
-------------------------

data Env =
  Env {
    connection :: PQ.Connection,
    executionEnv :: Execution.Env,
    nameCounter :: IORef (Maybe Word16)
  }

newEnv :: PQ.Connection -> IO Env
newEnv c =
  Env <$> pure c <*> Execution.newEnv c <*> newIORef Nothing


-- * Monad
-------------------------

newtype M r =
  M (ReaderT Env (EitherT Error IO) r)
  deriving (Functor, Applicative, Monad, MonadIO)

data Error =
  NotInTransaction |
  ExecutionError Execution.Error

run :: Env -> M r -> IO (Either Error r)
run e (M m) =
  runEitherT $ runReaderT m e

throwError :: Error -> M a
throwError e = M $ lift $ left $ e

liftExecution :: Execution.M a -> M a
liftExecution m =
  M $ ReaderT $ \e ->
    EitherT $ fmap (either (Left . ExecutionError) Right) $ 
    Execution.run (executionEnv e) m

-- |
-- Requires to be in transaction.
nextName :: M ByteString
nextName =
  do
    e <- M $ ask
    transactionState <- liftIO $ readIORef (nameCounter e)
    counter <- maybe (throwError NotInTransaction) return transactionState
    liftIO $ writeIORef (nameCounter e) (Just $ succ counter)
    return $ fromString $ 'x' : show counter

-- |
-- Returns a cursor identifier.
declareCursor :: Statement.Statement -> M Statement.Cursor
declareCursor s =
  do
    name <- nextName
    liftExecution $ 
      Execution.unitResult =<< 
      Execution.statement (Statement.declareCursor name s)
    return name

fetchFromCursor :: Statement.Cursor -> M (Vector (Vector (Maybe ByteString)))
fetchFromCursor cursor =
  liftExecution $
    Execution.vectorResult =<< 
    Execution.statement (Statement.fetchFromCursor cursor)

beginTransaction :: Statement.TransactionMode -> M ()
beginTransaction mode =
  do
    e <- M $ ask
    liftIO $ writeIORef (nameCounter e) (Just 0)
    liftExecution $ 
      Execution.unitResult =<< 
      Execution.statement (Statement.beginTransaction mode)

finishTransaction :: Bool -> M ()
finishTransaction commit =
  do
    liftExecution $ 
      Execution.unitResult =<< 
      Execution.statement (bool Statement.abortTransaction Statement.commitTransaction commit)
    e <- M $ ask
    liftIO $ writeIORef (nameCounter e) Nothing


-- * Stream
-------------------------

type Stream =
  ListT M (Vector (Maybe ByteString))

streamWithCursor :: Statement.Statement -> Stream
streamWithCursor statement =
  do
    cursor <- lift $ declareCursor statement
    let loop = do
          chunk <- lift $ fetchFromCursor cursor
          guard $ not $ Vector.null chunk
          Vector.foldl step mempty chunk <> loop
        step z r = z <> pure r
        in loop

