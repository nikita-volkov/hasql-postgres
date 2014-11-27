module Hasql.Postgres.Session where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Postgres.PTI as PTI
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.TemplateConverter as TemplateConverter
import qualified Hasql.Postgres.Session.StatementPreparation as StatementPreparation
import qualified Hasql.Postgres.Session.ResultProcessing as ResultProcessing
import qualified ListT
import qualified Data.Vector as Vector
import qualified Data.Text.Encoding as TE


-- * Environment
-------------------------

type Env =
  (PQ.Connection, IORef (Maybe Word), StatementPreparation.Env)

newEnv :: PQ.Connection -> IO Env
newEnv c =
  (,,) <$> pure c <*> newIORef Nothing <*> StatementPreparation.newEnv c


-- * Errors
-------------------------

data Error =
  NotInTransaction |
  UnexpectedResult Text |
  ErroneousResult Text |
  UnparsableTemplate Text |
  TransactionConflict
  deriving (Show, Typeable)

throwError :: Monad m => Error -> M m r
throwError =
  M . lift . lift . lift . left

-- * Session
-------------------------

newtype M m r =
  M (ResultProcessing.M 
      (StatementPreparation.M 
        (ReaderT (PQ.Connection, IORef (Maybe Word)) 
          (EitherT Error m))) 
      r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans M where
  lift = M . lift . lift . lift . lift

type Session r =
  MonadIO m => M m r

type Stream =
  MonadIO m => ListT (M m) (Vector (Maybe ByteString))

run :: MonadIO m => Env -> M m r -> m (Either Error r)
run (c, s, e1) (M m) =
  runEitherT $ 
  join $ liftM (EitherT . return . mapLeft localizeError . join) $ 
  flip runReaderT (c, s) $ StatementPreparation.run e1 $ ResultProcessing.run c $ m
  where
    mapLeft f = either (Left . f) Right
    localizeError = undefined

statement :: Statement.Statement -> Session (Maybe PQ.Result)
statement s =
  do
    (connection, _) <- M $ lift $ lift $ ask
    let (template, params, preparable) = s
    convertedTemplate <-
      either (throwError . convertTemplateError template) return $ TemplateConverter.convert template
    case preparable of
      True -> do
        let (tl, vl) = unzip params
        key <- M $ lift $ StatementPreparation.prepare convertedTemplate tl
        liftIO $ PQ.execPrepared connection key vl PQ.Binary
      False -> do
        let params' = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) params
        liftIO $ PQ.execParams connection convertedTemplate params' PQ.Binary
  where
    convertTemplateError t m = 
      UnparsableTemplate $ 
        "Template: " <> TE.decodeLatin1 t <> ". " <>
        "Error: " <> m <> "."


type ResultProcessing r =
  MonadIO m => Maybe PQ.Result -> M m r

{-# INLINE unitResult #-}
unitResult :: ResultProcessing ()
unitResult =
  M . (ResultProcessing.unit <=< ResultProcessing.just)

{-# INLINE matrixResult #-}
matrixResult :: ResultProcessing (Vector (Vector (Maybe ByteString)))
matrixResult =
  M . (ResultProcessing.vector <=< ResultProcessing.just)

{-# INLINE rowsAffectedResult #-}
rowsAffectedResult :: ResultProcessing Word64
rowsAffectedResult =
  M . (ResultProcessing.count <=< ResultProcessing.just)

-- |
-- Requires to be in transaction.
nextName :: Session ByteString
nextName =
  do
    (_, transactionStateRef) <- M $ lift $ lift $ ask
    transactionState <- liftIO $ readIORef transactionStateRef
    counter <- maybe (throwError NotInTransaction) return transactionState
    liftIO $ writeIORef transactionStateRef (Just $ succ counter)
    return $ fromString $ 'x' : show counter

-- |
-- Returns a cursor identifier.
declareCursor :: Statement.Statement -> Session Statement.Cursor
declareCursor s =
  do
    name <- nextName
    unitResult =<< statement (Statement.declareCursor name s)
    return name

closeCursor :: Statement.Cursor -> Session ()
closeCursor cursor =
  unitResult =<< statement (Statement.closeCursor cursor)

fetchFromCursor :: Statement.Cursor -> Session (Vector (Vector (Maybe ByteString)))
fetchFromCursor cursor =
  matrixResult =<< statement (Statement.fetchFromCursor cursor)

beginTransaction :: Statement.TransactionMode -> Session ()
beginTransaction mode =
  do
    (_, transactionStateRef) <- M $ lift $ lift $ ask
    liftIO $ writeIORef transactionStateRef (Just 0)
    unitResult =<< statement (Statement.beginTransaction mode)

finishTransaction :: Bool -> Session ()
finishTransaction commit =
  do
    unitResult =<< statement (bool Statement.abortTransaction Statement.commitTransaction commit)
    (_, transactionStateRef) <- M $ lift $ lift $ ask
    liftIO $ writeIORef transactionStateRef Nothing

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


