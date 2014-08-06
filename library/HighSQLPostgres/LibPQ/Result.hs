module HighSQLPostgres.LibPQ.Result where

import HighSQLPostgres.Prelude
import qualified Database.PostgreSQL.LibPQ as L
import qualified ListT


-- |
-- Either a failure with no result but some description or a comprehensive one.
data Failure =
  NoResult 
    (Maybe ByteString) |
  -- | Status, state, message, detail, hint.
  ResultFailure 
    ResultFailureStatus (Maybe ByteString) (Maybe ByteString) (Maybe ByteString) (Maybe ByteString)
  deriving (Show, Typeable)
  

data ResultFailureStatus =
  BadResponse | NonfatalError | FatalError
  deriving (Show, Typeable, Eq, Ord, Enum, Bounded)


data Success =
  RowsAffectedNum !ByteString |
  Stream !Stream


parse :: L.Connection -> Maybe L.Result -> IO (Either Failure (Maybe Success))
parse c =
  \case
    Nothing ->
      Left . NoResult <$> L.errorMessage c
    Just r ->
      L.resultStatus r >>=
        \case
          L.CommandOk ->
            Right . fmap RowsAffectedNum <$> L.cmdTuples r
          L.TuplesOk ->
            Right . Just . Stream <$> stream r
          L.BadResponse ->
            Left <$> statusFailure BadResponse
          L.NonfatalError ->
            Left <$> statusFailure NonfatalError
          L.FatalError ->
            Left <$> statusFailure FatalError
          r ->
            $bug $ "Unsupported result status: " <> show r
      where
        statusFailure s =
          ResultFailure s <$> state <*> message <*> detail <*> hint
          where
            state   = L.resultErrorField r L.DiagSqlstate
            message = L.resultErrorField r L.DiagMessagePrimary
            detail  = L.resultErrorField r L.DiagMessageDetail
            hint    = L.resultErrorField r L.DiagMessageHint

-- |
-- A width of a row and a stream of cells.
type Stream =
  (Int, ListT IO (Maybe ByteString))


stream :: L.Result -> IO Stream
stream r =
  do
    rows <- L.ntuples r
    cols <- L.nfields r
    return (colToInt cols, loop (pred cols) (pred rows) (pred cols))
  where
    colToInt (L.Col n) = fromIntegral n
    loop cnt rn cn =
      if rn >= 0 && cn >= 0
        then lift (L.getvalue r rn cn) >>= \v -> ListT.cons v next
        else mzero
      where
        next =
          case cn of
            0 -> loop cnt (pred rn) cnt
            _ -> loop cnt rn (pred cn)

