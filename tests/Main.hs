{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude hiding (assert)
import MTLPrelude
import Test.Framework
import Test.QuickCheck
import Test.QuickCheck.Instances
import Hasql
import Hasql.Postgres (Postgres(..))
import Data.Time
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified ListT
import qualified SlaveThread
import qualified Control.Concurrent.SSem as SSem
import qualified Hasql.Backend as Backend
import qualified Hasql as H
import qualified Hasql.Postgres as H
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector


type Text = Data.Text.Text
type LazyText = Data.Text.Lazy.Text
type ByteString = Data.ByteString.ByteString
type LazyByteString = Data.ByteString.Lazy.ByteString
type Scientific = Scientific.Scientific

main = 
  htfMain $ htf_thisModulesTests


test_sameStatementUsedOnDifferentTypes =
  session1 $ do
    liftIO . assertEqual (Just (Identity ("abc" :: Text))) =<< do 
      H.tx Nothing $ H.single $ [H.q|SELECT ?|] ("abc" :: Text)
    liftIO . assertEqual (Just (Identity True)) =<< do 
      H.tx Nothing $ H.single $ [H.q|SELECT ?|] True

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


-- * Mappings
-------------------------

test_mappingOfMaybe =
  session1 $ do
    validMappingSession (Just '!')
    validMappingSession (Nothing :: Maybe Bool)

test_mappingOfVector1 =
  session1 $ do
    validMappingSession v1
    validMappingSession v2
  where
    v1  = [v2, v2]
    v2  = [Just 'a', Nothing, Just 'b']

test_mappingOfList2 =
  session1 $ do
    validMappingSession v1
    validMappingSession v2
  where
    v1  = [v2, v2]
    v2  = [Just (1 :: Int), Just 2, Nothing]

test_mappingOfList3 =
  session1 $ do
    validMappingSession [" 'a' \"b\" \\c\\ " :: Text]

test_mappingOfList4 =
  assertEqual (Just (Identity v1)) =<< do
    session1 $ tx Nothing $ do
      unit $ [q|DROP TABLE IF EXISTS a|]
      unit $ [q|CREATE TABLE a ("v" char[][])|]
      unit $ [q|INSERT INTO a (v) VALUES (?)|] v1
      single $ [q|SELECT v FROM a|]
  where
    v1 =
      [
        [Just 'a', Just 'b'],
        [Nothing, Just 'c']
      ]

test_mappingOfList5 =
  session1 $ do
    validMappingSession [" 'a' \"b\" \\c\\ ф" :: ByteString]

prop_mappingOfListOverByteString (x :: [ByteString]) =
  mappingProp x

prop_mappingOfListOverLazyByteString (x :: [LazyByteString]) =
  mappingProp x

prop_mappingOfLazyText (v :: LazyText) =
  (isNothing $ Data.Text.Lazy.find (== '\NUL') v) ==>
    mappingProp v

prop_mappingOfLazyByteString (v :: LazyByteString) =
  mappingProp v


-- * Helpers
-------------------------

-- ** Generators
-------------------------

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

-- ** Session
-------------------------

selectSelf :: 
  Backend.Mapping Postgres a => 
  a -> (forall s. Session Postgres s IO (Maybe a))
selectSelf v =
  tx Nothing $ (fmap . fmap) runIdentity $ single $ [q| SELECT ? |] v

validMappingSession :: 
  Backend.Mapping Postgres a => Typeable a => Show a => Eq a => 
  a -> (forall s. Session Postgres s IO ())
validMappingSession v =
  selectSelf v >>= liftIO . assertEqual (Just v)

session1 :: (forall s. Session Postgres s IO r) -> IO r
session1 =
  session backendSettings poolSettings
  where
    backendSettings = PostgresParams "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ sessionSettings 6 30

-- ** Property
-------------------------

floatEqProp :: RealFrac a => Show a => a -> a -> Property
floatEqProp a b =
  counterexample (show a ++ " /~ " ++ show b) $
    a + error >= b && a - error <= b
  where
    error = max (abs a) 1 / 100

mappingProp :: (Show a, Eq a, Backend.Mapping Postgres a) => a -> Property
mappingProp v =
  Right v === do 
    unsafePerformIO $ (try :: IO a -> IO (Either H.Error a)) $ fmap fromJust $ 
      session1 $ selectSelf v
