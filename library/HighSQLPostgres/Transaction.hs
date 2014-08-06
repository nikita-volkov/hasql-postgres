module HighSQLPostgres.Transaction where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parsers as Parsers
import qualified HighSQLPostgres.Renderers as Renderers
import qualified HighSQLPostgres.Connection as C


-- |
-- A transaction state.
data Transaction =
  Transaction {
    newName :: C.M ByteString,
    -- | The bool indicates, whether to commit changes or rollback.
    finish :: Bool -> C.M ()
  }


data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 


data Mode =
  ReadWrite |
  ReadOnly


begin :: C.Connection -> Maybe Isolation -> Maybe Mode -> C.M Transaction
begin c i m = 
  do
    namesCounter <- liftIO $ newIORef 0
    let
      begin :: C.M ()
      begin =
        C.execute c (Renderers.run (i, m) beginStatementRenderer) [] []
      finish :: Bool -> C.M ()
      finish b =
        C.execute c s [] []
        where
          s = if b then "COMMIT" else "ABORT"
      newName :: C.M ByteString
      newName = 
        liftIO $ do
          n <- readIORef namesCounter
          writeIORef namesCounter (succ n)
          return $! Renderers.run n r
        where
          r n = Renderers.char 'v' <> Renderers.word16 n
      in begin >> return (Transaction newName finish)


-- * Renderers
-------------------------

beginStatementRenderer :: Renderers.R (Maybe Isolation, Maybe Mode)
beginStatementRenderer =
  \(i, m) ->
    "BEGIN" <> 
    maybe "" ((" " <>) . isolationRenderer) i <> 
    maybe "" ((" " <>) . modeRenderer) m

isolationRenderer :: Renderers.R Isolation
isolationRenderer =
  \case
    ReadCommitted  -> "ISOLATION LEVEL READ COMMITTED"
    RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
    Serializable   -> "ISOLATION LEVEL SERIALIZABLE"

modeRenderer :: Renderers.R Mode
modeRenderer =
  \case
    ReadWrite -> "READ WRITE"
    ReadOnly  -> "READ ONLY"
