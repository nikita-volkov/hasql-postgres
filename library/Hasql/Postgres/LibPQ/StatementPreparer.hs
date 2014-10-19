-- |
-- A component, which prepares statements.
module Hasql.Postgres.LibPQ.StatementPreparer where

import Hasql.Postgres.Prelude
import qualified Data.HashTable.IO as Hashtables
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Postgres.Renderer as Renderer
import qualified Hasql.Postgres.LibPQ.Result as LibPQ.Result



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

prepare :: ByteString -> [LibPQ.Oid] -> StatementPreparer -> ExceptT LibPQ.Result.Error IO RemoteKey
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
            LibPQ.Result.CommandOK _ -> return ()
            _ -> $bug $ "Unexpected result"
          liftIO $ Hashtables.insert table k n
          liftIO $ writeIORef counter (succ w)
          return n