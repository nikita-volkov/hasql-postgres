-- |
-- A backend-aware component, which prepares statements.
module Hasql.Postgres.StatementPreparer where

import Hasql.Postgres.Prelude
import qualified Data.HashTable.IO as Hashtables
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Postgres.ResultParser as Result
import qualified Hasql.Postgres.ResultHandler as ResultHandler
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Data.ByteString.Lazy as BL



type StatementPreparer =
  (PQ.Connection, IORef Word16, Hashtables.BasicHashTable LocalKey RemoteKey)


-- |
-- Local statement key.
data LocalKey =
  LocalKey !ByteString ![PQ.Oid]
  deriving (Show, Eq)

-- |
-- Optimized by ignoring the OIDs.
instance Hashable LocalKey where
  hashWithSalt s (LocalKey b _) = hashWithSalt s b


-- |
-- Remote statement key.
type RemoteKey =
  ByteString


new :: PQ.Connection -> IO StatementPreparer
new connection =
  (,,) <$> pure connection <*> newIORef 0 <*> Hashtables.new

prepare :: ByteString -> [PQ.Oid] -> StatementPreparer -> IO RemoteKey
prepare s tl (c, counter, table) =
  do
    let k = LocalKey s tl
    r <- Hashtables.lookup table k
    case r of
      Just r -> 
        return r
      Nothing ->
        do
          w <- readIORef counter
          n <- return (BL.toStrict $ BB.toLazyByteString $ BB.word16Dec w)
          ResultHandler.unit =<< Result.parse c =<< PQ.prepare c n s (partial (not . null) tl)
          Hashtables.insert table k n
          writeIORef counter (succ w)
          return n
