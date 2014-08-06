module HighSQLPostgres.Connection where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parsers as Parsers
import qualified HighSQLPostgres.Renderers as Renderers
import qualified HighSQLPostgres.LibPQ.Result as Result
import qualified HighSQLPostgres.LibPQ.Connection as Connection


-- |
-- A raw statement.
type Stmt =
  ByteString


type OID =
  L.Oid


-- |
-- Maybe a rendered value with its serialization format.
-- 'Nothing' implies @NULL@.
type StmtArg = 
  Maybe (ByteString, L.Format)


-- |
-- A width of a row and a stream of serialized values.
type ResultsStream =
  (Int, ListT IO ByteString)


data Connection =
  Connection {
    connection :: !L.Connection,
    stmtCounter :: !(IORef Word16), 
    stmtTable :: !(Hashtables.BasicHashTable LocStmtKey RemStmtKey)
  }


-- |
-- Local statement key.
data LocStmtKey =
  LocStmtKey !ByteString ![L.Oid]
  deriving (Show, Eq)

-- |
-- Optimized by ignoring the OIDs.
instance Hashable LocStmtKey where
  hashWithSalt s (LocStmtKey b _) = hashWithSalt s b


-- |
-- Remote statement key.
type RemStmtKey =
  ByteString


data Failure =
  ResultFailure Result.Failure |
  ParserFailure Text
  deriving (Show, Typeable)

type M =
  ExceptT Failure IO


establish :: Connection.Settings -> ExceptT Connection.Failure IO Connection
establish s =
  Connection <$> 
    Connection.new s <*> 
    lift (newIORef 0) <*>
    lift Hashtables.new

close :: Connection -> IO ()
close c =
  Connection.close (connection c)

prepare :: Connection -> Stmt -> [OID] -> M RemStmtKey
prepare c s tl =
  do
    r <- liftIO $ Hashtables.lookup (stmtTable c) k
    case r of
      Just r -> 
        return r
      Nothing ->
        do
          w <- liftIO $ readIORef (stmtCounter c)
          n <- return (Renderers.run w Renderers.word16)
          r <- parseResult c =<< do liftIO $ L.prepare (connection c) n s (partial (not . null) tl)
          case r of
            Nothing -> return ()
            _ -> $bug "Unexpected result"
          liftIO $ Hashtables.insert (stmtTable c) k n
          liftIO $ writeIORef (stmtCounter c) (succ w)
          return n
  where
    k = LocStmtKey s tl

parseResult :: Connection -> Maybe L.Result -> M (Maybe Result.Success)
parseResult c =
  lift . Result.parse (connection c) >=> 
  either (throwError . ResultFailure) return

execute :: Connection -> Stmt -> [OID] -> [StmtArg] -> M ()
execute =
  $notImplemented

executeCountingEffects :: Connection -> Stmt -> [OID] -> [StmtArg] -> M Integer
executeCountingEffects c s tl al = 
  do
    n <- prepare c s tl
    r <- parseResult c =<< do liftIO $ L.execPrepared (connection c) n al L.Text
    case r of
      Just (Result.RowsAffectedNum r) ->
        either (throwError . ParserFailure) return $ Parsers.run r Parsers.integral
      _ ->
        $bug "Unexpected result"

type ResultStream =
  Result.Stream

executeStreaming :: Connection -> Stmt -> [OID] -> [StmtArg] -> M ResultStream 
executeStreaming c s tl al = 
  do
    n <- prepare c s tl
    r <- parseResult c =<< do liftIO $ L.execPrepared (connection c) n al L.Text
    case r of
      Just (Result.Stream r) ->
        return r
      _ ->
        $bug "Unexpected result"
