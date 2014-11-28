module Hasql.Postgres.Session.Execution where

import Hasql.Postgres.Prelude
import qualified Data.HashTable.IO as Hashtables
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Postgres.Statement as Statement
import qualified Hasql.Postgres.TemplateConverter as TemplateConverter
import qualified Hasql.Postgres.Session.ResultProcessing as ResultProcessing


-- * Environment
-------------------------

type Env =
  (PQ.Connection, IORef Word16, Hashtables.BasicHashTable LocalKey RemoteKey)

newEnv :: PQ.Connection -> IO Env
newEnv c =
  (,,) <$> pure c <*> newIORef 0 <*> Hashtables.new


-- |
-- Local statement key.
data LocalKey =
  LocalKey !ByteString ![Word32]
  deriving (Show, Eq)

instance Hashable LocalKey where
  hashWithSalt salt (LocalKey template types) =
    hashWithSalt salt template

localKey :: ByteString -> [PQ.Oid] -> LocalKey
localKey t ol =
  LocalKey t (map oidMapper ol)
  where
    oidMapper (PQ.Oid x) = fromIntegral x


-- |
-- Remote statement key.
type RemoteKey =
  ByteString


data Error =
  UnexpectedResult Text |
  ErroneousResult Text |
  UnparsableTemplate ByteString Text |
  TransactionConflict


-- * Monad
-------------------------

newtype M r =
  M (ReaderT Env (EitherT Error IO) r)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: Env -> M r -> IO (Either Error r)
run e (M m) =
  runEitherT $ runReaderT m e

throwError :: Error -> M a
throwError = M . lift . left

prepare :: ByteString -> [PQ.Oid] -> M RemoteKey
prepare s tl =
  do
    (c, counter, table) <- M $ ask
    let lk = localKey s tl
    rk <- liftIO $ Hashtables.lookup table lk
    ($ rk) $ ($ return) $ maybe $ do
      w <- liftIO $ readIORef counter
      let rk = fromString $ show w
      unitResult =<< do liftIO $ PQ.prepare c rk s (partial (not . null) tl)
      liftIO $ Hashtables.insert table lk rk
      liftIO $ writeIORef counter (succ w)
      return rk

statement :: Statement.Statement -> M (Maybe PQ.Result)
statement s =
  do
    (c, _, _) <- M $ ask
    let (template, params, preparable) = s
    convertedTemplate <-
      either (throwError . UnparsableTemplate template) return $ 
      TemplateConverter.convert template
    case preparable of
      True -> do
        let (tl, vl) = unzip params
        key <- prepare convertedTemplate tl
        liftIO $ PQ.execPrepared c key vl PQ.Binary
      False -> do
        let params' = map (\(t, v) -> (\(vb, vf) -> (t, vb, vf)) <$> v) params
        liftIO $ PQ.execParams c convertedTemplate params' PQ.Binary

liftResultProcessing :: ResultProcessing.M a -> M a
liftResultProcessing m =
  M $ ReaderT $ \(c, _, _) -> 
    EitherT $ fmap (either (Left . mapError) Right) $ ResultProcessing.run c m
  where
    mapError =
      \case
        ResultProcessing.UnexpectedResult t   -> UnexpectedResult t
        ResultProcessing.ErroneousResult t    -> ErroneousResult t
        ResultProcessing.TransactionConflict  -> TransactionConflict

{-# INLINE unitResult #-}
unitResult :: Maybe PQ.Result -> M ()
unitResult =
  liftResultProcessing . (ResultProcessing.unit <=< ResultProcessing.just)

{-# INLINE vectorResult #-}
vectorResult :: Maybe PQ.Result -> M (Vector (Vector (Maybe ByteString)))
vectorResult =
  liftResultProcessing . (ResultProcessing.vector <=< ResultProcessing.just)

{-# INLINE countResult #-}
countResult :: Maybe PQ.Result -> M Word64
countResult =
  liftResultProcessing . (ResultProcessing.count <=< ResultProcessing.just)

