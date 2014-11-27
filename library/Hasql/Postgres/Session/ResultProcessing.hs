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


type M m =
  EitherT Error (ReaderT PQ.Connection m)

type Unlift m r = 
  M m r -> m (Either Error r)

data Error =
  UnexpectedResult Text |
  ErroneousResult Text |
  TransactionConflict

run :: Monad m => PQ.Connection -> m (Unlift m r)
run c =
  return $ \m -> flip runReaderT c $ runEitherT m

result :: MonadIO m => Maybe PQ.Result -> M m PQ.Result
result =
  ($ return) $ maybe $ do
    m <- lift $ ask >>= liftIO . PQ.errorMessage
    left $ ErroneousResult $ case m of
      Nothing -> 
        "Sending a command to the server failed"
      Just m ->
        "Sending a command to the server failed due to: " <> 
        TE.decodeLatin1 m

statusGuard :: MonadIO m => (PQ.ExecStatus -> Bool) -> PQ.Result -> M m ()
statusGuard g r =
  do
    s <- liftIO $ PQ.resultStatus r
    unless (g s) $ do
      case s of
        PQ.BadResponse   -> failWithErroneousResult "Bad response"
        PQ.NonfatalError -> failWithErroneousResult "Non-fatal error"
        PQ.FatalError    -> failWithErroneousResult "Fatal error"
        _ -> left $ UnexpectedResult $ "Unexpected result status: " <> (fromString $ show s)
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
            in when transactionConflict $ left $ TransactionConflict
        message <- liftIO $ PQ.resultErrorField r PQ.DiagMessagePrimary
        detail <- liftIO $ PQ.resultErrorField r PQ.DiagMessageDetail
        hint <- liftIO $ PQ.resultErrorField r PQ.DiagMessageHint
        left $ ErroneousResult $ erroneousResultMessage status code message detail hint
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

-- match :: (PQ.ExecStatus -> PQ.Result -> Maybe (T a)) -> Maybe PQ.Result -> T a
-- match matcher mr = 
--   do
--     r <- result mr
--     s <- liftIO $ PQ.resultStatus r
--     case matcher s r of
--       Just rp -> rp
--       Nothing -> do
--         erroneousStatusGuard s r
--         lift $ left $ UnexpectedResult $ "Unexpected result status: " <> (fromString $ show s)

-- resolveResult :: (PQ.ExecStatus -> Bool) -> Maybe PQ.Result -> T (PQ.Result, PQ.ExecStatus)
-- resolveResult statusChecker mr =
--   do
--     r <- result mr
--     s <- liftIO $ PQ.resultStatus r
--     unless (statusChecker s) $ do
--       erroneousStatusGuard s r
--       lift $ left $ UnexpectedResult $ "Unexpected result status: " <> (fromString $ show s)
--     return (r, s)

-- unit :: Maybe PQ.Result -> T ()
-- unit =
--   match $ \case
--     PQ.CommandOk -> const $ Just $ return ()
--     _ -> const $ Nothing

unit :: MonadIO m => Maybe PQ.Result -> M m ()
unit mr =
  do
    r <- result mr
    statusGuard (\case PQ.CommandOk -> True; _ -> False) r
    return ()

-- -- count :: Maybe PQ.Result -> T Word64
-- -- count =
-- --   match $ \case
-- --     PQ.CommandOk -> 

vector :: MonadIO m => Maybe PQ.Result -> M m (Vector (Vector (Maybe ByteString)))
vector mr =
  do
    r <- result mr
    statusGuard (\case PQ.TuplesOk -> True; _ -> False) r
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
