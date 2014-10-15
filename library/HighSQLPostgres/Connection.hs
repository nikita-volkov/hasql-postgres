module HighSQLPostgres.Connection where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parser as Parser
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.LibPQ.Result as Result
import qualified HighSQLPostgres.LibPQ.Connector as Connector
import qualified HighSQLPostgres.LibPQ.StatementPreparer as StatementPreparer



type Connection =
  (L.Connection, StatementPreparer.StatementPreparer, IORef (Maybe Transaction))

type Transaction =
  (Word)

-- |
-- A width of a row and a stream of serialized values.
type ResultsStream =
  (Int, ListT IO ByteString)

type Stmt =
  (ByteString, [(ValueType, Value)])

-- |
-- Maybe a rendered value with its serialization format.
-- 'Nothing' implies @NULL@.
type Value = 
  Maybe (ByteString, L.Format)

type ValueType =
  L.Oid

type Cursor =
  ByteString


-- * Transaction types
-------------------------

data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 

type Mode =
  (Isolation, Bool)


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

executePrepared :: Stmt -> Session (Maybe Result.Success)
executePrepared s =
  do
    (connection, preparer, _) <- ask
    let (bs, pl) = s
        (tl, vl) = unzip pl
    key <- lift $ withExceptT ResultError $ StatementPreparer.prepare bs tl preparer
    result <- liftIO $ L.execPrepared connection key vl L.Text
    parseResult result

execute :: Stmt -> Session (Maybe Result.Success)
execute s =
  do
    (connection, _, _) <- ask
    let (template, pl) = s
        parameters = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) pl
    result <- liftIO $ L.execParams connection template parameters L.Text
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
declareCursor :: Stmt -> Session Cursor
declareCursor (template, values) =
  do
    name <- nextName
    let
      template' =
        "DECLARE ? NO SCROLL CURSOR FOR " <> template
      values' =
        (OID.varchar, Just (name, L.Text)) : values
    executePrepared (template', values')
    return name

closeCursor :: Cursor -> Session ()
closeCursor cursor =
  unitResult =<< execute (template, [])
  where
    template =
      Renderer.run cursor $ \c -> Renderer.string7 "CLOSE " <> Renderer.byteString c

unitResult :: Maybe Result.Success -> Session ()
unitResult result =
  $notImplemented

streamResult :: Maybe Result.Success -> Session ResultsStream
streamResult result =
  $notImplemented

fetchFromCursor :: Cursor -> Session ResultsStream
fetchFromCursor cursor =
  streamResult =<< execute (template, [])
  where
    template =
      Renderer.run cursor $ \c -> 
        Renderer.string7 "FETCH FORWARD 256 FROM " <> Renderer.byteString c

beginTransaction :: Mode -> Session ()
beginTransaction mode =
  unitResult =<< execute (template, [])
  where
    template =
      Renderer.run mode $ \(i, w) ->
        mconcat $ intersperse (Renderer.char7 ' ') $
          [
            Renderer.string7 "BEGIN"
            ,
            case i of
              ReadCommitted  -> Renderer.string7 "ISOLATION LEVEL READ COMMITTED"
              RepeatableRead -> Renderer.string7 "ISOLATION LEVEL REPEATABLE READ"
              Serializable   -> Renderer.string7 "ISOLATION LEVEL SERIALIZABLE"
            ,
            case w of
              True  -> "READ WRITE"
              False -> "READ ONLY"
          ]


