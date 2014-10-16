{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (Read, Write, read, write, assert)
import MTLPrelude hiding (modify)
import Test.Framework
import Test.QuickCheck.Monadic
import HighSQL
import HighSQLPostgres (Postgres(..))
import qualified ListT


main = 
  htfMain $ htf_thisModulesTests


-- test_null
-- test_mappingOfMaybe

test_mappingOfBool =
  runSession $ do
    validMapping True
    validMapping False

prop_mappingOfChar =
  monadicIO $ do
    v :: Char <- pick arbitrary
    r <- 
      liftIO $ runSession $ withoutLocking $ do
        r <- select $ [q| SELECT ? |] v
        ListT.head r
    assert (Just v == r)

-- test_mappingOfText =
--   undefined

-- test_mappingOfInt =
--   undefined

-- test_mappingOfInt8 =
--   undefined

-- test_mappingOfInt16 =
--   undefined

-- test_mappingOfInt32 =
--   undefined

-- test_mappingOfInt64 =
--   undefined

-- test_mappingOfWord =
--   undefined

-- test_mappingOfWord8 =
--   undefined

-- test_mappingOfWord16 =
--   undefined

-- test_mappingOfWord32 =
--   undefined

-- test_mappingOfWord64 =
--   undefined

-- test_mappingOfDay =
--   undefined

-- test_mappingOfTimeOfDay =
--   undefined

-- test_mappingOfLocalTime =
--   undefined

-- test_mappingOfZonedTime =
--   undefined

-- test_mappingOfUTCTime =
--   undefined

validMapping :: RowParser Postgres a => Mapping Postgres a => Show a => Eq a => Typeable a => a -> Session ()
validMapping v =
  do
    r <- withoutLocking $ do
      r <- select $ [q| SELECT ? |] v
      ListT.head r
    liftIO $ assertEqual (Just v) r



-- * Session monad
-------------------------

type Session =
  ReaderT (Pool Postgres) IO

runSession :: Session r -> IO r
runSession reader =
  withPool backendSettings poolSettings $ runReaderT reader
  where
    backendSettings = Postgres "localhost" 5432 "postgres" "" "postgres"
    poolSettings = Settings 1 6 (30 * 10 ^ 12)

withoutLocking :: (forall s. Transaction Postgres WithoutLocking s r) -> Session r
withoutLocking t =
  ReaderT $ runWithoutLocking t

read :: IsolationLevel -> (forall s. Transaction Postgres Read s r) -> Session r
read i t =
  ReaderT $ runRead i t

write :: IsolationLevel -> (forall s. Transaction Postgres Write s r) -> Session r
write i t =
  ReaderT $ runWrite i t


