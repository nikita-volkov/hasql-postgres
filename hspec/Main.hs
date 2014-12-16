module Main where

import BasePrelude hiding (assert)
import MTLPrelude
import Test.Hspec
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
import qualified Hasql.Postgres as HP
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector


type Text = Data.Text.Text
type LazyText = Data.Text.Lazy.Text
type ByteString = Data.ByteString.ByteString
type LazyByteString = Data.ByteString.Lazy.ByteString
type Scientific = Scientific.Scientific


main = 
  hspec $ do

    describe "Feature" $ do
      
      it "wrongPort" $ do
        let 
          backendSettings = HP.ParamSettings "localhost" 1 "postgres" "" "postgres"
          poolSettings = fromJust $ sessionSettings 6 30
          io =
            H.session backendSettings poolSettings $ do
              H.tx Nothing $ H.unit [H.q|DROP TABLE IF EXISTS a|]
          in
            shouldThrow io $ \case
              H.CantConnect _ -> True
              _ -> False

      it "sameStatementUsedOnDifferentTypes" $ do
        session1 $ do
          liftIO . (flip shouldBe) (Just (Identity ("abc" :: Text))) =<< do 
            H.tx Nothing $ H.single $ [H.q|SELECT ?|] ("abc" :: Text)
          liftIO . (flip shouldBe) (Just (Identity True)) =<< do 
            H.tx Nothing $ H.single $ [H.q|SELECT ?|] True

      it "rendering" $ do
        let rows = [("A", 34525), ("B", 324987)] :: [(Text, Int)]
        (flip shouldBe) (Just $ head rows) =<< do 
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

      it "countEffects" $ do
        (flip shouldBe) 100 =<< do 
          session1 $ do
            tx Nothing $ do
              unit [q|DROP TABLE IF EXISTS a|]
              unit [q|CREATE TABLE a (id SERIAL NOT NULL, name VARCHAR NOT NULL)|]
              replicateM_ 100 $ do
                unit [q|INSERT INTO a (name) VALUES ('a')|]
              count [q|DELETE FROM a|]

      it "autoIncrement" $ do
        (flip shouldBe) (Just (1 :: Word64), Just (2 :: Word64)) =<< do
          session1 $ tx Nothing $ do
            unit [q|DROP TABLE IF EXISTS a|]
            unit [q|CREATE TABLE a (id SERIAL NOT NULL, v INT8, PRIMARY KEY (id))|]
            id1 <- (fmap . fmap) runIdentity $ single $ [q|INSERT INTO a (v) VALUES (1) RETURNING id|]
            id2 <- (fmap . fmap) runIdentity $ single $ [q|INSERT INTO a (v) VALUES (2) RETURNING id|]
            return (id1, id2)

      it "cursorResultsOrder" $ do
        session1 $ do
          r :: [Word] <-
            tx (Just (ReadCommitted, False)) $ do
              ListT.toList $ fmap runIdentity $ stream $ 
                [q|select oid from pg_type ORDER BY oid|]
          liftIO $ (flip shouldBe) (sort r) r

      it "cursor" $ do
        session1 $ do
          r :: [(Word, Text)] <-
            tx (Just (ReadCommitted, False)) $ do
              ListT.toList $ stream $
                [q|select oid, typname from pg_type|]
          r' :: [(Word, Text)] <-
            tx (Just (ReadCommitted, False)) $ do
              list $ [q|select oid, typname from pg_type|]
          liftIO $ (flip shouldBe) r' r

      it "select" $ do
        session1 $ do
          r :: [(Word, Text)] <-
            tx Nothing $ do
              list $ [q|select oid, typname from pg_type|]
          liftIO $ (flip shouldBe) True $ not $ null r

    describe "Mapping of" $ do

      describe "Enum" $ do
        it "casting" $ do
          session1 $ do
            H.tx Nothing $ do
              H.unit [H.q| DROP TYPE IF EXISTS mood |]
              H.unit [H.q| CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy') |]
            liftIO . shouldBe (Just (Identity ("ok" :: Text))) =<< do 
              H.tx Nothing $ H.single $ [H.q|SELECT (? :: mood)|] ("ok" :: Text)
              
        it "table" $ do
          session1 $ do
            H.tx Nothing $ do
              H.unit [H.q| DROP TABLE IF EXISTS a |]
              H.unit [H.q| DROP TYPE IF EXISTS mood |]
              H.unit [H.q| CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy') |]
              H.unit [H.q| CREATE TABLE a (id SERIAL NOT NULL, 
                                           mood mood NOT NULL,
                                           PRIMARY KEY (id)) |]
              H.unit [H.q| INSERT INTO a (mood) VALUES ('ok') |]
              H.unit [H.q| INSERT INTO a (mood) VALUES ('ok') |]
              H.unit [H.q| INSERT INTO a (mood) VALUES ('happy') |]
            liftIO . shouldBe ([1, 2] :: [Int]) . fmap runIdentity =<< do 
              H.tx Nothing $ H.list $ [H.q|SELECT id FROM a WHERE mood = ?|] ("ok" :: Text)

      describe "Maybe" $ do
        it "" $ do
          session1 $ do
            validMappingSession (Just '!')
            validMappingSession (Nothing :: Maybe Bool)
      
      describe "List" $ do

        it "1" $ do
          let
            v1  = [v2, v2]
            v2  = [Just 'a', Nothing, Just 'b']
          session1 $ do
            validMappingSession v1
            validMappingSession v2

        it "2" $ do
          let
            v1  = [v2, v2]
            v2  = [Just (1 :: Int), Just 2, Nothing]
          session1 $ do
            validMappingSession v1
            validMappingSession v2

        it "3" $ do
          session1 $ do
            validMappingSession [" 'a' \"b\" \\c\\ " :: Text]

        it "4" $ do
          let
            v1 =
              [
                [Just 'a', Just 'b'],
                [Nothing, Just 'c']
              ]
          (flip shouldBe) (Just (Identity v1)) =<< do
            session1 $ tx Nothing $ do
              unit $ [q|DROP TABLE IF EXISTS a|]
              unit $ [q|CREATE TABLE a ("v" char[][])|]
              unit $ [q|INSERT INTO a (v) VALUES (?)|] v1
              single $ [q|SELECT v FROM a|]

        it "5" $ do
          session1 $ do
            validMappingSession [" 'a' \"b\" \\c\\ ф" :: ByteString]

        it "ByteString" $ property $ \(x :: [ByteString]) ->
          mappingProp x

        it "LazyByteString" $ property $ \(x :: [LazyByteString]) ->
          mappingProp x

      it "LazyText" $ property $ \(v :: LazyText) ->
        (isNothing $ Data.Text.Lazy.find (== '\NUL') v) ==>
          mappingProp v

      it "LazyByteString" $ property $ \(v :: LazyByteString) ->
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
  a -> Session Postgres s IO (Maybe a)
selectSelf v =
  tx Nothing $ (fmap . fmap) runIdentity $ single $ [q| SELECT ? |] v

session1 :: (forall s. Session Postgres s IO r) -> IO r
session1 =
  session backendSettings poolSettings
  where
    backendSettings = HP.ParamSettings "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ sessionSettings 6 30

validMappingSession :: 
  Backend.Mapping Postgres a => Typeable a => Show a => Eq a => 
  a -> Session Postgres s IO ()
validMappingSession v =
  selectSelf v >>= liftIO . (flip shouldBe) (Just v)

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
