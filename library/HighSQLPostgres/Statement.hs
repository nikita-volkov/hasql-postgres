module HighSQLPostgres.Statement where

import HighSQLPostgres.Prelude
import qualified Database.PostgreSQL.LibPQ as L
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Renderer as Renderer


type Statement =
  (ByteString, [(ValueType, Value)], Preparable)

-- |
-- Maybe a rendered value with its serialization format.
-- 'Nothing' implies @NULL@.
type Value = 
  Maybe (ByteString, L.Format)

type ValueType =
  L.Oid

type Preparable =
  Bool  


-- * Transaction types
-------------------------

type Cursor =
  ByteString

data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 

type TransactionMode =
  (Isolation, Bool)


declareCursor :: Cursor -> Statement -> Statement
declareCursor cursor (template, values, preparable) =
  let
    template' =
      "DECLARE ? NO SCROLL CURSOR FOR " <> template
    values' =
      (OID.varchar, Just (cursor, L.Text)) : values
    in (template', values', preparable)

closeCursor :: Cursor -> Statement
closeCursor cursor =
  (template, [], False)
  where
    template =
      Renderer.run cursor $ \c -> Renderer.string7 "CLOSE " <> Renderer.byteString c

fetchFromCursor :: Cursor -> Statement
fetchFromCursor cursor =
  (template, [], False)
  where
    template =
      Renderer.run cursor $ \c -> 
        Renderer.string7 "FETCH FORWARD 256 FROM " <> Renderer.byteString c

beginTransaction :: TransactionMode -> Statement
beginTransaction mode =
  (template, [], True)
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

commitTransaction :: Statement
commitTransaction =
  ("COMMIT", [], True)

abortTransaction :: Statement
abortTransaction =
  ("ABORT", [], True)


