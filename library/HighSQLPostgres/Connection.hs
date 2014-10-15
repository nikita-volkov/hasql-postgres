module HighSQLPostgres.Connection where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parser as Parser
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.Statement as Statement
import qualified HighSQLPostgres.LibPQ.Result as Result
import qualified HighSQLPostgres.LibPQ.Connector as Connector
import qualified HighSQLPostgres.LibPQ.StatementPreparer as StatementPreparer


type Connection =
  (L.Connection, StatementPreparer.StatementPreparer, IORef (Maybe Word))

-- |
-- A width of a row and a stream of serialized values.
type ResultsStream =
  (Int, ListT IO ByteString)


-- * Errors
-------------------------

data Error =
  NotInTransaction |
  UnexpectedResult |
  ResultError Result.Error
  deriving (Show, Typeable)

instance Exception Error


-- * Session
-------------------------

type Session =
  ReaderT Connection (ExceptT Error IO)

-- |
-- Execute the session, throwing the exceptions.
runSession :: Connection -> Session r -> IO r
runSession c s =
  join $ fmap (either throwIO return) $ runExceptT $ runReaderT s c

parseResult :: Maybe L.Result -> Session (Maybe Result.Success)
parseResult r =
  ReaderT $ \(c, _, _) -> lift (Result.parse c r) >>= either (throwError . ResultError) return

execute :: Statement.Statement -> Session (Maybe Result.Success)
execute s =
  do
    result <- do
      (connection, preparer, _) <- ask
      let (template, params, preparable) = s
      case preparable of
        True -> do
          let (tl, vl) = unzip params
          key <- lift $ withExceptT ResultError $ StatementPreparer.prepare template tl preparer
          liftIO $ L.execPrepared connection key vl L.Text
        False -> do
          let params' = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) params
          liftIO $ L.execParams connection template params' L.Text
    parseResult result

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

unitResult :: Maybe Result.Success -> Session ()
unitResult result =
  $notImplemented

streamResult :: Maybe Result.Success -> Session ResultsStream
streamResult result =
  $notImplemented

fetchFromCursor :: Statement.Cursor -> Session ResultsStream
fetchFromCursor cursor =
  streamResult =<< execute (Statement.fetchFromCursor cursor)

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


