-- |
-- A component, which prepares statements.
module HighSQLPostgres.LibPQ.StatementPreparer where

import HighSQLPostgres.Prelude
import qualified Data.HashTable.IO as Hashtables
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified HighSQLPostgres.Renderer as Renderer
import qualified HighSQLPostgres.LibPQ.Result as LibPQ.Result



type StatementPreparer =
  (LibPQ.Connection, IORef Word16, Hashtables.BasicHashTable LocalKey RemoteKey)


-- |
-- Local statement key.
data LocalKey =
  LocalKey !ByteString ![LibPQ.Oid]
  deriving (Show, Eq)

-- |
-- Optimized by ignoring the OIDs.
instance Hashable LocalKey where
  hashWithSalt s (LocalKey b _) = hashWithSalt s b


-- |
-- Remote statement key.
type RemoteKey =
  ByteString


new :: LibPQ.Connection -> IO StatementPreparer
new connection =
  (,,) <$> pure connection <*> newIORef 0 <*> Hashtables.new

prepare :: ByteString -> [LibPQ.Oid] -> StatementPreparer -> ExceptT LibPQ.Result.Failure IO RemoteKey
prepare s tl (c, counter, table) =
  do
    let k = LocalKey s tl
    r <- liftIO $ Hashtables.lookup table k
    case r of
      Just r -> 
        return r
      Nothing ->
        do
          w <- liftIO $ readIORef counter
          n <- return (Renderer.run w Renderer.word16)
          r <- do 
            r <- liftIO $ LibPQ.prepare c n s (partial (not . null) tl)
            ExceptT $ LibPQ.Result.parse c r
          case r of
            Nothing -> return ()
            _ -> $bug "Unexpected result"
          liftIO $ Hashtables.insert table k n
          liftIO $ writeIORef counter (succ w)
          return n
    

