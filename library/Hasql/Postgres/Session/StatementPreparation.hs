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
type Env =
  (PQ.Connection, IORef Word16, Hashtables.BasicHashTable LocalKey RemoteKey)

newtype M r =
  M (ReaderT Env ResultProcessing.M r)
  deriving (Functor, Applicative, Monad, MonadIO)

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


newEnv :: PQ.Connection -> IO Env
newEnv c =
  (,,) <$> pure c <*> newIORef 0 <*> Hashtables.new

run :: Env -> M r -> IO (Either ResultProcessing.Error r)
run e (M m) =
  let (c, _, _) = e
      in ResultProcessing.run c $ runReaderT m e

prepare :: ByteString -> [PQ.Oid] -> M RemoteKey
prepare s tl =
  M $ ReaderT $ \(c, counter, table) -> do
    let k = LocalKey s tl
    nm <- liftIO $ Hashtables.lookup table k
    case nm of
      Just n -> 
        return n
      Nothing ->
        do
          w <- liftIO $ readIORef counter
          let n = BL.toStrict $ BB.toLazyByteString $ BB.word16Dec w
          ResultProcessing.unit =<< ResultProcessing.just =<< do 
            liftIO $ PQ.prepare c n s (partial (not . null) tl)
          liftIO $ Hashtables.insert table k n
          liftIO $ writeIORef counter (succ w)
          return n


