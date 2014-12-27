module Hasql.Postgres.Statement where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Postgres.Statement.TemplateConverter as TC


data Statement =
  Statement Template [(ValueType, Value)] Preparable
  deriving (Show, Eq, Generic)

data Template =
  PreparedTemplate ByteString |
  UnicodeTemplate Text
  deriving (Show, Eq, Generic)

instance Hashable Template

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

preparedTemplate :: Template -> ByteString
preparedTemplate =
  \case
    PreparedTemplate b -> b
    UnicodeTemplate t -> BL.toStrict $ BB.toLazyByteString $ TC.convert t

preparedTemplateBuilder :: Template -> BB.Builder
preparedTemplateBuilder =
  \case
    PreparedTemplate b -> BB.byteString b
    UnicodeTemplate t -> TC.convert t

declareCursor :: Cursor -> Statement -> Statement
declareCursor cursor (Statement template values preparable) =
  let
    template' =
      PreparedTemplate $ BL.toStrict $ BB.toLazyByteString $
        BB.string7 "DECLARE " <> BB.byteString cursor <> BB.char7 ' ' <>
        BB.string7 "NO SCROLL CURSOR FOR " <> preparedTemplateBuilder template
    in Statement template' values preparable

closeCursor :: Cursor -> Statement
closeCursor cursor =
  Statement template [] True
  where
    template =
      PreparedTemplate $ "CLOSE " <> cursor

fetchFromCursor :: Cursor -> Statement
fetchFromCursor cursor =
  Statement template [] True
  where
    template =
      PreparedTemplate $ "FETCH FORWARD 256 FROM " <> cursor

beginTransaction :: TransactionMode -> Statement
beginTransaction (i, w) =
  Statement template [] True
  where
    template =
      PreparedTemplate $ 
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
          True  -> BB.string7 "READ WRITE"
          False -> BB.string7 "READ ONLY"
      ]

commitTransaction :: Statement
commitTransaction =
  Statement (PreparedTemplate "COMMIT") [] True

abortTransaction :: Statement
abortTransaction =
  Statement (PreparedTemplate "ABORT") [] True


