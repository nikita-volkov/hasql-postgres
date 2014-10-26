module Hasql.Postgres.ResultParser
( 
  Result(..), 
  StatusErrorStatus(..),
  RowsStream(..),
  RowsVector(..),
  RowsList(..),
  parse,
  erroneousResultText,
)
where

import Hasql.Postgres.Prelude
import qualified Database.PostgreSQL.LibPQ as L
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified ListT
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text


data Result =
  -- |
  -- Out-of-memory conditions or serious errors such as inability to send the command to the server.
  -- May contain some description.
  NoResult (Maybe ByteString) |
  -- |
  -- A failure with comprehensive description.
  -- 
  -- The fields are: status, code, message, detail, hint.
  StatusError StatusErrorStatus ByteString (Maybe ByteString) (Maybe ByteString) (Maybe ByteString) |
  -- |
  -- Command executed fine.
  -- 
  -- The fields are: a number of affected rows.
  CommandOK (Maybe ByteString) |
  -- |
  -- Command executed fine and returns rows.
  -- 
  -- The fields are generators of respective rows representations.
  Rows (IO RowsStream) (IO RowsVector) (IO RowsList)

data StatusErrorStatus =
  BadResponse | NonfatalError | FatalError
  deriving (Show, Typeable, Eq, Ord, Enum, Bounded)


parse :: L.Connection -> Maybe L.Result -> IO Result
parse c =
  \case
    Nothing ->
      NoResult <$> L.errorMessage c
    Just r ->
      L.resultStatus r >>=
        \case
          L.CommandOk ->
            CommandOK <$> L.cmdTuples r
          L.TuplesOk ->
            return $ Rows <$> getRowsStream <*> getRowsVector <*> getRowsList $ r
          L.BadResponse ->
            statusError BadResponse
          L.NonfatalError ->
            statusError NonfatalError
          L.FatalError ->
            statusError FatalError
          r ->
            $bug $ "Unsupported result status: " <> show r
      where
        statusError s =
          StatusError s <$> state <*> message <*> detail <*> hint
          where
            state   = fromJust <$> L.resultErrorField r L.DiagSqlstate
            message = L.resultErrorField r L.DiagMessagePrimary
            detail  = L.resultErrorField r L.DiagMessageDetail
            hint    = L.resultErrorField r L.DiagMessageHint


{-# INLINE erroneousResultText #-}
erroneousResultText :: Result -> Maybe Text
erroneousResultText =
  \case
    NoResult (Just bs) -> 
      Just $ "Inable to send command to the server due to: " <> Text.decodeLatin1 bs
    NoResult Nothing -> 
      Just $ "Inable to send command to the server"
    StatusError status code message details hint ->
      Just $ 
        "A status error. " <> formatFields fields
      where
        formatFields = 
          formatList . map formatField . catMaybes
          where
            formatList items =
              Text.intercalate "; " items <> "."
            formatField (n, v) =
              n <> ": \"" <> v <> "\""
        fields =
          [
            Just ("Status", fromString $ show status),
            Just ("Code", Text.decodeLatin1 code),
            fmap (("Message",) . Text.decodeLatin1) $ message,
            fmap (("Details",) . Text.decodeLatin1) $ details,
            fmap (("Hint",) . Text.decodeLatin1) $ hint
          ]
    _ -> 
      Nothing



-- * Rows processing
-------------------------

type Row =
  Vector (Maybe ByteString)


type RowsStream =
  ListT IO Row

getRowsStream :: L.Result -> IO RowsStream
getRowsStream r =
  {-# SCC "getRowsStream" #-} 
  do
    nr <- L.ntuples r
    nc <- L.nfields r
    return $ 
      let
        loop ir = 
          if ir < nr
            then do 
              row <- 
                liftIO $ do
                  mv <- MVector.new (colInt nc)
                  forM_ [0..pred nc] $ \ic ->
                    MVector.write mv (colInt ic) =<< L.getvalue r ir ic
                  Vector.unsafeFreeze mv
              ListT.cons row (loop (succ ir))
            else mzero
        in 
          loop 0


type RowsVector =
  Vector Row

getRowsVector :: L.Result -> IO RowsVector
getRowsVector r =
  {-# SCC "getRowsVector" #-} 
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


type RowsList =
  [Row]

getRowsList :: L.Result -> IO RowsList
getRowsList r =
  {-# SCC "getRowsList" #-} 
  do
    nr <- L.ntuples r
    nc <- L.nfields r
    mvx <- MVector.new (rowInt nr)
    forM [0..pred nr] $ \ir -> do
      mvy <- MVector.new (colInt nc)
      forM_ [0..pred nc] $ \ic -> do
        MVector.write mvy (colInt ic) =<< L.getvalue r ir ic
      Vector.unsafeFreeze mvy


-- * Utils
-------------------------

{-# INLINE colInt #-}
colInt :: L.Column -> Int
colInt (L.Col n) = fromIntegral n

{-# INLINE rowInt #-}
rowInt :: L.Row -> Int
rowInt (L.Row n) = fromIntegral n
