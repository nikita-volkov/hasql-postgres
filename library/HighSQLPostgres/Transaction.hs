module HighSQLPostgres.Transaction where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Data.ByteString as ByteString
import qualified Data.HashTable.IO as Hashtables
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Parser as Parser
import qualified HighSQLPostgres.Renderer as Renderer
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
        C.execute c (Renderer.run (i, m) beginStatementRenderer) [] []
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
          return $! Renderer.run n r
        where
          r n = Renderer.char 'v' <> Renderer.word16 n
      in begin >> return (Transaction newName finish)


-- * Renderer
-------------------------

beginStatementRenderer :: Renderer.R (Maybe Isolation, Maybe Mode)
beginStatementRenderer =
  \(i, m) ->
    "BEGIN" <> 
    maybe "" ((" " <>) . isolationRenderer) i <> 
    maybe "" ((" " <>) . modeRenderer) m

isolationRenderer :: Renderer.R Isolation
isolationRenderer =
  \case
    ReadCommitted  -> "ISOLATION LEVEL READ COMMITTED"
    RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
    Serializable   -> "ISOLATION LEVEL SERIALIZABLE"

modeRenderer :: Renderer.R Mode
modeRenderer =
  \case
    ReadWrite -> "READ WRITE"
    ReadOnly  -> "READ ONLY"
