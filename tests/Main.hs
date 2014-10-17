{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (Read, Write, read, write, assert)
import MTLPrelude hiding (modify)
import Test.Framework
import Test.QuickCheck.Instances
import HighSQL
import HighSQLPostgres (Postgres(..))
import Data.Text (Text)
import Data.Time
import qualified Data.Text
import qualified ListT


main = 
  htfMain $ htf_thisModulesTests


-- test_null
-- test_mappingOfMaybe

test_mappingOfBool =
  runSession $ do
    validMappingSession True
    validMappingSession False

test_mappingOfUTF8Char =
  runSession $ do
    validMappingSession 'Й'

-- Postgres does not allow the '/NUL' character in text data
prop_mappingOfChar (v :: Char) =
  (v /= '\NUL') ==>
    Just v === do unsafePerformIO $ runSession $ selectSelf v

-- Postgres does not allow the '/NUL' character in text data
prop_mappingOfText (v :: Text) =
  (isNothing $ Data.Text.find (== '\NUL') v) ==>
    Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfInt (v :: Int) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfInt8 (v :: Int8) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfInt16 (v :: Int16) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfInt32 (v :: Int32) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfInt64 (v :: Int64) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfWord (v :: Word) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfWord8 (v :: Word8) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfWord16 (v :: Word16) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfWord32 (v :: Word32) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfWord64 (v :: Word64) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfDay (v :: Day) =
  Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfTimeOfDay (v :: TimeOfDay) =
  forAll microsTimeOfDayGen $ \v -> 
    Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfLocalTime =
  forAll microsLocalTimeGen $ \v -> 
    Just v === do unsafePerformIO $ runSession $ selectSelf v

prop_mappingOfZonedTime =
  forAll gen $ \v -> 
    eq v $ fromJust $ do unsafePerformIO $ runSession $ selectSelf v
  where
    eq a b = zonedTimeToUTC a === zonedTimeToUTC b
    gen =
      do
        t <- microsLocalTimeGen
        z <- minutesToTimeZone <$> choose (negate (11 * 60 + 59), 11 * 60 + 59)
        return $ ZonedTime t z

prop_mappingOfUTCTime =
  forAll gen $ \v ->
    Just v === do unsafePerformIO $ runSession $ selectSelf v
  where
    gen = UTCTime <$> arbitrary <*> microsDiffTimeGen

microsTimeOfDayGen :: Gen TimeOfDay
microsTimeOfDayGen =
  timeToTimeOfDay <$> microsDiffTimeGen

microsLocalTimeGen :: Gen LocalTime
microsLocalTimeGen = 
  LocalTime <$> arbitrary <*> microsTimeOfDayGen

microsDiffTimeGen :: Gen DiffTime
microsDiffTimeGen = do
  fmap picosecondsToDiffTime $ fmap (* (10^6)) $ choose (0, (10^6)*24*60*60)

selectSelf :: RowParser Postgres a => Mapping Postgres a => Typeable a => a -> Session (Maybe a)
selectSelf v =
  withoutLocking $ do
    r <- select $ [q| SELECT ? |] v
    ListT.head r

validMappingSession :: 
  RowParser Postgres a => Mapping Postgres a => Typeable a => Show a => Eq a => 
  a -> Session ()
validMappingSession v =
  selectSelf v >>= liftIO . assertEqual (Just v)


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


