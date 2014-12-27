module Hasql.Postgres.Session.ResultProcessing where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Hasql.Postgres.ErrorCode as ErrorCode
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as B
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector


newtype M r =
  M (EitherT Error (ReaderT PQ.Connection IO) r)
  deriving (Functor, Applicative, Monad, MonadIO)

data Error =
  -- |
  -- Received no response from the database.
  NoResult !(Maybe ByteString) |
  -- | 
  -- An error reported by the DB. Code, message, details, hint.
  -- 
  -- * The SQLSTATE code for the error. The SQLSTATE code identifies the type of error that has occurred; it can be used by front-end applications to perform specific operations (such as error handling) in response to a particular database error. For a list of the possible SQLSTATE codes, see Appendix A. This field is not localizable, and is always present.
  -- * The primary human-readable error message (typically one line). Always present.
  -- * Detail: an optional secondary error message carrying more detail about the problem. Might run to multiple lines.
  -- * Hint: an optional suggestion what to do about the problem. This is intended to differ from detail in that it offers advice (potentially inappropriate) rather than hard facts. Might run to multiple lines.
  ErroneousResult !ByteString !ByteString !(Maybe ByteString) !(Maybe ByteString) |
  -- |
  -- The database returned an unexpected result.
  -- Indicates an improper statement or a schema mismatch.
  UnexpectedResult !Text |
  TransactionConflict
  deriving (Show)

run :: PQ.Connection -> M r -> IO (Either Error r)
run e (M m) =
  flip runReaderT e $ runEitherT m

just :: Maybe PQ.Result -> M PQ.Result
just =
  ($ return) $ maybe $ M $ do
    m <- lift $ ask >>= liftIO . PQ.errorMessage
    left $ NoResult $ m

checkStatus :: (PQ.ExecStatus -> Bool) -> PQ.Result -> M ()
checkStatus g r =
  do
    s <- liftIO $ PQ.resultStatus r
    unless (g s) $ do
      case s of
        PQ.BadResponse   -> failWithErroneousResult
        PQ.NonfatalError -> failWithErroneousResult
        PQ.FatalError    -> failWithErroneousResult
        _ -> M $ left $ UnexpectedResult $ "Unexpected result status: " <> (fromString $ show s)
  where
    failWithErroneousResult =
      do
        code <- 
          fmap (fromMaybe ($bug "No code")) $
          liftIO $ PQ.resultErrorField r PQ.DiagSqlstate
        let transactionConflict = code == ErrorCode.serialization_failure
        when transactionConflict $ M $ left $ TransactionConflict
        message <- 
          fmap (fromMaybe ($bug "No message")) $
          liftIO $ PQ.resultErrorField r PQ.DiagMessagePrimary
        detail <- 
          liftIO $ PQ.resultErrorField r PQ.DiagMessageDetail
        hint <- 
          liftIO $ PQ.resultErrorField r PQ.DiagMessageHint
        M $ left $ ErroneousResult code message detail hint


unit :: PQ.Result -> M ()
unit r =
  checkStatus (\case PQ.CommandOk -> True; PQ.TuplesOk -> True; _ -> False) r

count :: PQ.Result -> M Word64
count r =
  do  checkStatus (\case PQ.CommandOk -> True; _ -> False) r
      r' <- liftIO $ PQ.cmdTuples r
      maybe (M $ left $ UnexpectedResult $ "No number of affected rows")
            (parseWord64)
            (mfilter (not . B.null) r')

parseWord64 :: ByteString -> M Word64
parseWord64 b =
  either (\m -> M $ left $ UnexpectedResult $ "Couldn't parse Word64: " <> fromString m)
         (return)
         (Atto.parseOnly (Atto.decimal <* Atto.endOfInput) b)

vector :: PQ.Result -> M (Vector (Vector (Maybe ByteString)))
vector r =
  do
    checkStatus (\case PQ.TuplesOk -> True; _ -> False) r
    liftIO $ do
      nr <- PQ.ntuples r
      nc <- PQ.nfields r
      mvx <- MVector.unsafeNew (rowInt nr)
      forM_ [0..pred nr] $ \ir -> do
        mvy <- MVector.unsafeNew (colInt nc)
        forM_ [0..pred nc] $ \ic -> do
          MVector.unsafeWrite mvy (colInt ic) =<< PQ.getvalue r ir ic
        vy <- Vector.unsafeFreeze mvy
        MVector.unsafeWrite mvx (rowInt ir) vy
      Vector.unsafeFreeze mvx

{-# INLINE colInt #-}
colInt :: PQ.Column -> Int
colInt (PQ.Col n) = fromIntegral n

{-# INLINE rowInt #-}
rowInt :: PQ.Row -> Int
rowInt (PQ.Row n) = fromIntegral n
