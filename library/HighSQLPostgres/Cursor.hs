module HighSQLPostgres.Cursor where

import HighSQLPostgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified HighSQLPostgres.OID as OID
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.Connection as C
import qualified HighSQLPostgres.Transaction as T
import qualified Data.ByteString as ByteString
import qualified ListT


-- |
data Cursor =
  Cursor {
    stream :: C.M Stream,
    close :: C.M ()
  }


-- |
-- A width of a row and a stream of cells.
type Stream =
  (Int, ListT C.M (Maybe ByteString))


type Failure = 
  C.Failure


declare :: C.Connection -> T.Transaction -> C.Stmt -> [LibPQ.Oid] -> [C.StmtArg] -> C.M Cursor
declare c t s tl al = 
  do
    n <- T.newName t
    let
      declare =
        do
          C.execute c s' tl' al'
        where
          al' = 
            Just (n, LibPQ.Text) : al
          s'  = 
            "DECLARE ? NO SCROLL CURSOR FOR " <> s
          tl' = 
            OID.varchar : tl
      close = 
        C.execute c "CLOSE ?" [OID.varchar] [Just (n, LibPQ.Text)]
      fetch = 
        C.executeStreaming c s tl al 
        where
          s =
            "FETCH FORWARD ? FROM ?"
          tl = 
            [OID.int2, OID.varchar]
          al = 
            [Just (Renderer.run 256 Renderer.word16, LibPQ.Text), 
             Just (n, LibPQ.Text)]
      stream =
        do
          (w, s) <- fetch
          return (w, listT s)
          where
            listT l = 
              do
                lift (ListT.null l') >>= 
                  \case
                    True ->
                      mzero
                    False ->
                      l' <> (lift fetch >>= listT . snd)
              where
                l' = hoist liftIO l
      in
        declare >> return (Cursor stream close)
