module HighSQLPostgres.Session where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as ByteString
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parser as Parser
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.LibPQ.Result as Result
import qualified HighSQLPostgres.LibPQ.Connector as Connector
import qualified HighSQLPostgres.LibPQ.StatementPreparer as StatementPreparer



type Session r =
  ReaderT 
    (L.Connection, StatementPreparer.StatementPreparer) 
    (StateT (Maybe Transaction) (ExceptT Result.Error IO))
    r

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


-- executePrepared :: Stmt -> 

-- parseResult :: Maybe L.Result -> Session 

streamWithCursor :: Stmt -> Session ResultsStream
streamWithCursor s =
  do
    
    $notImplemented

