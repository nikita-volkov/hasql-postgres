-- |
-- A backend-aware component, which prepares statements.
module Hasql.Postgres.Session.StatementPreparation where

import Hasql.Postgres.Prelude
import qualified Data.HashTable.IO as Hashtables
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Postgres.Session.ResultProcessing as ResultProcessing

-- |
-- Environment
type E =
  (PQ.Connection, IORef Word16, Hashtables.BasicHashTable LocalKey RemoteKey)

type M m =
  ReaderT E (ResultProcessing.M m)

-- |
-- Local statement key.
data LocalKey =
  LocalKey !ByteString ![PQ.Oid]
  deriving (Show, Eq, Generic)

instance Hashable LocalKey

-- |
-- Remote statement key.
type RemoteKey =
  ByteString

run :: MonadIO m => PQ.Connection -> M m r -> m (Either ResultProcessing.Error r)
run c m =
  do  e <- liftIO $ (,,) <$> pure c <*> newIORef 0 <*> Hashtables.new
      ResultProcessing.run c $ runReaderT m e

prepare :: MonadIO m => ByteString -> [PQ.Oid] -> M m RemoteKey
prepare s tl =
  ReaderT $ \(c, counter, table) -> do
    let k = LocalKey s tl
    nm <- liftIO $ Hashtables.lookup table k
    case nm of
      Just n -> 
        return n
      Nothing ->
        do
          w <- liftIO $ readIORef counter
          let n = BL.toStrict $ BB.toLazyByteString $ BB.word16Dec w
          ResultProcessing.unit =<< do liftIO $ PQ.prepare c n s (partial (not . null) tl)
          liftIO $ Hashtables.insert table k n
          liftIO $ writeIORef counter (succ w)
          return n


