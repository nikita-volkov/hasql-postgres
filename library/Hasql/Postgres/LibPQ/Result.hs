module Hasql.Postgres.LibPQ.Result where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as L
import qualified ListT
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector


-- |
-- Either a failure with no result but some description or a comprehensive one.
data Error =
  NoResult 
    (Maybe ByteString) |
  -- | Status, state, message, detail, hint.
  ResultError 
    ResultErrorStatus ByteString (Maybe ByteString) (Maybe ByteString) (Maybe ByteString)
  deriving (Show, Typeable)
  

data ResultErrorStatus =
  BadResponse | NonfatalError | FatalError
  deriving (Show, Typeable, Eq, Ord, Enum, Bounded)


data Success =
  CommandOK !(Maybe ByteString) |
  Matrix !Matrix


parse :: L.Connection -> Maybe L.Result -> IO (Either Error Success)
parse c =
  \case
    Nothing ->
      Left . NoResult <$> L.errorMessage c
    Just r ->
      L.resultStatus r >>=
        \case
          L.CommandOk ->
            Right . CommandOK <$> L.cmdTuples r
          L.TuplesOk ->
            Right . Matrix <$> getMatrix r
          L.BadResponse ->
            Left <$> statusError BadResponse
          L.NonfatalError ->
            Left <$> statusError NonfatalError
          L.FatalError ->
            Left <$> statusError FatalError
          r ->
            $bug $ "Unsupported result status: " <> show r
      where
        statusError s =
          ResultError s <$> state <*> message <*> detail <*> hint
          where
            state   = fromJust <$> L.resultErrorField r L.DiagSqlstate
            message = L.resultErrorField r L.DiagMessagePrimary
            detail  = L.resultErrorField r L.DiagMessageDetail
            hint    = L.resultErrorField r L.DiagMessageHint


type Stream =
  ListT IO (Vector (Maybe ByteString))

getStream :: L.Result -> IO Stream
getStream r =
  do
    rows <- L.ntuples r
    cols <- L.nfields r
    return $ 
      let
        loop ri = 
          if ri < rows
            then do 
              row <- 
                liftIO $ do
                  mv <- MVector.new (colInt cols)
                  forM_ [0..pred cols] $ \ci ->
                    MVector.write mv (colInt ci) =<< L.getvalue r ri ci
                  Vector.unsafeFreeze mv
              ListT.cons row (loop (succ ri))
            else mzero
        in 
          loop 0
  where
    colInt (L.Col n) = fromIntegral n


type Matrix =
  Vector (Vector (Maybe ByteString))

getMatrix :: L.Result -> IO Matrix
getMatrix r =
  do
    nr <- L.ntuples r
    nc <- L.nfields r
    mvx <- MVector.new (rowInt nr)
    forM_ [0..pred nr] $ \ir -> do
      mvy <- MVector.new (colInt nc)
      forM_ [0..pred nc] $ \ic -> do
        MVector.write mvy (colInt ic) =<< L.getvalue r ir ic
      vy <- Vector.unsafeFreeze mvy
      MVector.write mvx (rowInt ir) vy
    Vector.unsafeFreeze mvx
  where
    colInt (L.Col n) = fromIntegral n
    rowInt (L.Row n) = fromIntegral n
