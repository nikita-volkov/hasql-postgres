module Hasql.Postgres.Session.ResultProcessing where

import Hasql.Postgres.Prelude hiding (fail)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Postgres.ErrorCode as ErrorCode
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified ListT


newtype M m r =
  M (EitherT Error (ReaderT PQ.Connection m) r)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans M where
  lift = M . lift . lift

data Error =
  UnexpectedResult Text |
  ErroneousResult Text |
  TransactionConflict

run :: Monad m => PQ.Connection -> M m r -> m (Either Error r)
run e (M m) =
  flip runReaderT e $ runEitherT m

just :: MonadIO m => Maybe PQ.Result -> M m PQ.Result
just =
  ($ return) $ maybe $ M $ do
    m <- lift $ ask >>= liftIO . PQ.errorMessage
    left $ ErroneousResult $ case m of
      Nothing -> 
        "Sending a command to the server failed"
      Just m ->
        "Sending a command to the server failed due to: " <> 
        TE.decodeLatin1 m

checkStatus :: MonadIO m => (PQ.ExecStatus -> Bool) -> PQ.Result -> M m ()
checkStatus g r =
  do
    s <- liftIO $ PQ.resultStatus r
    unless (g s) $ do
      case s of
        PQ.BadResponse   -> failWithErroneousResult "Bad response"
        PQ.NonfatalError -> failWithErroneousResult "Non-fatal error"
        PQ.FatalError    -> failWithErroneousResult "Fatal error"
        _ -> M $ left $ UnexpectedResult $ "Unexpected result status: " <> (fromString $ show s)
  where
    failWithErroneousResult status =
      do
        code <- liftIO $ PQ.resultErrorField r PQ.DiagSqlstate
        let transactionConflict =
              case code of
                Just x -> 
                  elem x $
                  [
                    ErrorCode.transaction_rollback,
                    ErrorCode.transaction_integrity_constraint_violation,
                    ErrorCode.serialization_failure,
                    ErrorCode.statement_completion_unknown,
                    ErrorCode.deadlock_detected
                  ]
                Nothing ->
                  False
            in when transactionConflict $ M $ left $ TransactionConflict
        message <- liftIO $ PQ.resultErrorField r PQ.DiagMessagePrimary
        detail <- liftIO $ PQ.resultErrorField r PQ.DiagMessageDetail
        hint <- liftIO $ PQ.resultErrorField r PQ.DiagMessageHint
        M $ left $ ErroneousResult $ erroneousResultMessage status code message detail hint
    erroneousResultMessage status code message details hint =
      formatFields fields
      where
        formatFields = 
          formatList . map formatField . catMaybes
          where
            formatList items =
              T.intercalate "; " items <> "."
            formatField (n, v) =
              n <> ": \"" <> v <> "\""
        fields =
          [
            Just ("Status", fromString $ show status),
            fmap (("Code",) . TE.decodeLatin1) $ code,
            fmap (("Message",) . TE.decodeLatin1) $ message,
            fmap (("Details",) . TE.decodeLatin1) $ details,
            fmap (("Hint",) . TE.decodeLatin1) $ hint
          ]

unit :: MonadIO m => PQ.Result -> M m ()
unit r =
  checkStatus (\case PQ.CommandOk -> True; _ -> False) r

count :: MonadIO m => PQ.Result -> M m Word64
count r =
  do  checkStatus (\case PQ.CommandOk -> True; _ -> False) r
      (liftIO $ PQ.cmdTuples r) >>= 
        maybe (M $ left $ UnexpectedResult $ "No number of affected rows")
              (parseWord64)

parseWord64 :: Monad m => ByteString -> M m Word64
parseWord64 b =
  either (\m -> M $ left $ UnexpectedResult $ "Couldn't parse Word64: " <> fromString m)
         (return)
         (Atto.parseOnly (Atto.decimal <* Atto.endOfInput) b)

vector :: MonadIO m => PQ.Result -> M m (Vector (Vector (Maybe ByteString)))
vector r =
  do
    checkStatus (\case PQ.TuplesOk -> True; _ -> False) r
    liftIO $ do
      nr <- PQ.ntuples r
      nc <- PQ.nfields r
      mvx <- MVector.new (rowInt nr)
      forM_ [0..pred nr] $ \ir -> do
        mvy <- MVector.new (colInt nc)
        forM_ [0..pred nc] $ \ic -> do
          MVector.write mvy (colInt ic) =<< PQ.getvalue r ir ic
        vy <- Vector.unsafeFreeze mvy
        MVector.write mvx (rowInt ir) vy
      Vector.unsafeFreeze mvx

{-# INLINE colInt #-}
colInt :: PQ.Column -> Int
colInt (PQ.Col n) = fromIntegral n

{-# INLINE rowInt #-}
rowInt :: PQ.Row -> Int
rowInt (PQ.Row n) = fromIntegral n
