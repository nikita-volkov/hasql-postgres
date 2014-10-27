{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (assert)
import MTLPrelude
import Test.Framework
import Test.QuickCheck.Instances
import Hasql
import Hasql.Postgres (Postgres(..))
import Data.Text (Text)
import Data.Time
import Data.Scientific (Scientific)
import qualified Data.Text
import qualified ListT
import qualified SlaveThread
import qualified Control.Concurrent.SSem as SSem
import qualified Hasql.Backend as Backend
import qualified Hasql as H
import qualified Hasql.Postgres as H
import qualified Data.Scientific as Scientific


main = 
  htfMain $ htf_thisModulesTests


test_rendering =
  assertEqual (Just $ head rows) =<< do 
    session1 $ do
      H.tx Nothing $ do
        H.unit [H.q|DROP TABLE IF EXISTS a|]
        H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, 
                                    name VARCHAR NOT NULL, 
                                    birthday INT8,
                                    PRIMARY KEY (id))|]
        forM_ rows $ \(name, birthday) -> do
          H.unit $ [H.q|INSERT INTO a (name, birthday) VALUES (?, ?)|] name birthday
      H.tx Nothing $ do
        H.single $ [H.q|SELECT name, birthday FROM a WHERE id = ? |] (1 :: Int)
  where
    rows = [("A", 34525), ("B", 324987)] :: [(Text, Int)]

test_countEffects =
  assertEqual 100 =<< do 
    session1 $ do
      tx Nothing $ do
        unit [q|DROP TABLE IF EXISTS a|]
        unit [q|CREATE TABLE a (id SERIAL NOT NULL, name VARCHAR NOT NULL)|]
        replicateM_ 100 $ do
          unit [q|INSERT INTO a (name) VALUES ('a')|]
        count [q|DELETE FROM a|]

test_autoIncrement =
  assertEqual (Just (1 :: Word64), Just (2 :: Word64)) =<< do
    session1 $ tx Nothing $ do
      unit [q|DROP TABLE IF EXISTS a|]
      unit [q|CREATE TABLE a (id SERIAL NOT NULL, v INT8, PRIMARY KEY (id))|]
      id1 <- (fmap . fmap) runIdentity $ single $ [q|INSERT INTO a (v) VALUES (1) RETURNING id|]
      id2 <- (fmap . fmap) runIdentity $ single $ [q|INSERT INTO a (v) VALUES (2) RETURNING id|]
      return (id1, id2)

test_transactionConflictResolution =
  do
    session1 $ tx Nothing $ do
      unit [q|DROP TABLE IF EXISTS a|]
      unit [q|CREATE TABLE a ("id" int8, "v" int8, PRIMARY KEY ("id"))|]
      unit [q|INSERT INTO a (id, v) VALUES ('7', '0')|]
    semaphore <- SSem.new (-1)
    SlaveThread.fork $ session >> SSem.signal semaphore
    SlaveThread.fork $ session >> SSem.signal semaphore
    SSem.wait semaphore
    r <- session1 $ tx Nothing $ single [q|SELECT v FROM a WHERE id='7'|]
    assertEqual (Just (Identity (200 :: Int))) r
  where
    session =
      session1 $ do
        replicateM 100 $ tx (Just (Serializable, True)) $ do
          Just (Identity (v :: Int)) <- single [q|SELECT v FROM a WHERE id='7'|]
          unit $ [q|UPDATE a SET v=? WHERE id='7'|] (succ v)

test_transaction =
  unitTestPending ""

test_cursorResultsOrder =
  session1 $ do
    r :: [Word] <-
      tx (Just (ReadCommitted, False)) $ do
        ListT.toList $ fmap runIdentity $ stream $ 
          [q|select oid from pg_type ORDER BY oid|]
    liftIO $ assertEqual (sort r) r

test_cursor =
  session1 $ do
    r :: [(Word, Text)] <-
      tx (Just (ReadCommitted, False)) $ do
        ListT.toList $ stream $
          [q|select oid, typname from pg_type|]
    r' :: [(Word, Text)] <-
      tx (Just (ReadCommitted, False)) $ do
        list $ [q|select oid, typname from pg_type|]
    liftIO $ assertEqual r' r

test_select =
  session1 $ do
    r :: [(Word, Text)] <-
      tx Nothing $ do
        list $ [q|select oid, typname from pg_type|]
    liftIO $ assertNotEqual [] r

test_mappingOfMaybe =
  session1 $ do
    validMappingSession (Just '!')
    validMappingSession (Nothing :: Maybe Bool)

test_mappingOfBool =
  session1 $ do
    validMappingSession True
    validMappingSession False

test_mappingOfUTF8Char =
  session1 $ do
    validMappingSession 'Й'

-- Postgres does not allow the '/NUL' character in text data
prop_mappingOfChar (v :: Char) =
  (v /= '\NUL') ==>
    Just v === do unsafePerformIO $ session1 $ selectSelf v

-- Postgres does not allow the '/NUL' character in text data
prop_mappingOfText (v :: Text) =
  (isNothing $ Data.Text.find (== '\NUL') v) ==>
    Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfInt (v :: Int) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfInt8 (v :: Int8) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfInt16 (v :: Int16) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfInt32 (v :: Int32) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfInt64 (v :: Int64) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfWord (v :: Word) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfWord8 (v :: Word8) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfWord16 (v :: Word16) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfWord32 (v :: Word32) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfWord64 (v :: Word64) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfFloat (v :: Float) =
  floatEq v (fromJust $ unsafePerformIO $ session1 $ selectSelf v)

prop_mappingOfDouble (v :: Double) =
  floatEq v (fromJust $ unsafePerformIO $ session1 $ selectSelf v)

prop_mappingOfScientific =
  forAll scientificGen $ \v ->
    Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfDay (v :: Day) =
  Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfTimeOfDay (v :: TimeOfDay) =
  forAll microsTimeOfDayGen $ \v -> 
    Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfLocalTime =
  forAll microsLocalTimeGen $ \v -> 
    Just v === do unsafePerformIO $ session1 $ selectSelf v

prop_mappingOfZonedTime =
  forAll gen $ \v -> 
    eq v $ fromJust $ do unsafePerformIO $ session1 $ selectSelf v
  where
    eq a b = zonedTimeToUTC a === zonedTimeToUTC b
    gen =
      do
        t <- microsLocalTimeGen
        z <- minutesToTimeZone <$> choose (negate (11 * 60 + 59), 11 * 60 + 59)
        return $ ZonedTime t z

prop_mappingOfUTCTime =
  forAll gen $ \v ->
    Just v === do unsafePerformIO $ session1 $ selectSelf v
  where
    gen = UTCTime <$> arbitrary <*> microsDiffTimeGen

scientificGen :: Gen Scientific
scientificGen =
  Scientific.scientific <$> arbitrary <*> arbitrary

microsTimeOfDayGen :: Gen TimeOfDay
microsTimeOfDayGen =
  timeToTimeOfDay <$> microsDiffTimeGen

microsLocalTimeGen :: Gen LocalTime
microsLocalTimeGen = 
  LocalTime <$> arbitrary <*> microsTimeOfDayGen

microsDiffTimeGen :: Gen DiffTime
microsDiffTimeGen = do
  fmap picosecondsToDiffTime $ fmap (* (10^6)) $ choose (0, (10^6)*24*60*60)

selectSelf :: 
  Backend.Mapping Postgres a => Typeable a => 
  a -> Session Postgres IO (Maybe a)
selectSelf v =
  tx Nothing $ (fmap . fmap) runIdentity $ single $ [q| SELECT ? |] v

validMappingSession :: 
  Backend.Mapping Postgres a => Typeable a => Show a => Eq a => 
  a -> Session Postgres IO ()
validMappingSession v =
  selectSelf v >>= liftIO . assertEqual (Just v)

session1 :: Session Postgres IO r -> IO r
session1 =
  session backendSettings poolSettings
  where
    backendSettings = Postgres "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ sessionSettings 6 30

floatEq :: RealFrac a => Show a => a -> a -> Bool
floatEq a b =
  a + error > b && a - error < b
  where
    error = 1 / 100
