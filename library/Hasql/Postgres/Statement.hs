module Hasql.Postgres.Statement where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL


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
      "DECLARE " <> cursor <> " NO SCROLL CURSOR FOR " <> template
    in (template', values, preparable)

closeCursor :: Cursor -> Statement
closeCursor cursor =
  (template, [], False)
  where
    template =
      "CLOSE " <> cursor

fetchFromCursor :: Cursor -> Statement
fetchFromCursor cursor =
  (template, [], False)
  where
    template =
      "FETCH FORWARD 256 FROM " <> cursor

beginTransaction :: TransactionMode -> Statement
beginTransaction (i, w) =
  (template, [], True)
  where
    template =
      BL.toStrict $ BB.toLazyByteString $
        mconcat $ intersperse (BB.char7 ' ') $
          [
            BB.string7 "BEGIN"
            ,
            case i of
              ReadCommitted  -> BB.string7 "ISOLATION LEVEL READ COMMITTED"
              RepeatableRead -> BB.string7 "ISOLATION LEVEL REPEATABLE READ"
              Serializable   -> BB.string7 "ISOLATION LEVEL SERIALIZABLE"
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


