module Main where

import BasePrelude hiding (assert, isRight, isLeft)
import MTLPrelude
import Control.Monad.Trans.Either
import Data.Either.Combinators
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Time
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified ListT
import qualified Hasql.Backend as Backend
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified PostgreSQLBinary.Encoder as PBE
import qualified Data.Aeson as J


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
          poolSettings = fromJust $ H.poolSettings 6 30
          in do
            r <- 
              session backendSettings poolSettings $ do
                H.tx Nothing $ H.unitEx [H.stmt|DROP TABLE IF EXISTS a|]
            shouldSatisfy r $ \case
              Left (H.CxError _) -> True
              _ -> False

      it "sameStatementUsedOnDifferentTypes" $ do
        flip shouldSatisfy isRight =<< do
          session1 $ do
            liftIO . (flip shouldBe) (Just (Identity ("abc" :: Text))) =<< do 
              H.tx Nothing $ H.maybeEx $ [H.stmt|SELECT ?|] ("abc" :: Text)
            liftIO . (flip shouldBe) (Just (Identity True)) =<< do 
              H.tx Nothing $ H.maybeEx $ [H.stmt|SELECT ?|] True

      it "rendering" $ do
        let rows = [("A", 34525), ("B", 324987)] :: [(Text, Int)]
        (flip shouldBe) (Right $ Just $ head rows) =<< do 
          session1 $ do
            H.tx Nothing $ do
              H.unitEx [H.stmt|DROP TABLE IF EXISTS a|]
              H.unitEx [H.stmt|CREATE TABLE a (id SERIAL NOT NULL, 
                                               name VARCHAR NOT NULL, 
                                               birthday INT8,
                                               PRIMARY KEY (id))|]
              forM_ rows $ \(name, birthday) -> do
                H.unitEx $ [H.stmt|INSERT INTO a (name, birthday) VALUES (?, ?)|] name birthday
            H.tx Nothing $ do
              H.maybeEx $ [H.stmt|SELECT name, birthday FROM a WHERE id = ? |] (1 :: Int)

      it "countEffects" $ do
        (flip shouldBe) (Right 100) =<< do 
          session1 $ do
            H.tx Nothing $ do
              H.unitEx [H.stmt|DROP TABLE IF EXISTS a|]
              H.unitEx [H.stmt|CREATE TABLE a (id SERIAL NOT NULL, name VARCHAR NOT NULL)|]
              replicateM_ 100 $ do
                H.unitEx [H.stmt|INSERT INTO a (name) VALUES ('a')|]
              H.countEx [H.stmt|DELETE FROM a|]

      it "autoIncrement" $ do
        (flip shouldBe) (Right (Just (1 :: Word64), Just (2 :: Word64))) =<< do
          session1 $ H.tx Nothing $ do
            H.unitEx [H.stmt|DROP TABLE IF EXISTS a|]
            H.unitEx [H.stmt|CREATE TABLE a (id SERIAL NOT NULL, v INT8, PRIMARY KEY (id))|]
            id1 <- (fmap . fmap) runIdentity $ H.maybeEx $ [H.stmt|INSERT INTO a (v) VALUES (1) RETURNING id|]
            id2 <- (fmap . fmap) runIdentity $ H.maybeEx $ [H.stmt|INSERT INTO a (v) VALUES (2) RETURNING id|]
            return (id1, id2)

      it "cursorResultsOrder" $ do
        flip shouldSatisfy (\case Right r -> (sort r :: [Word]) == r; _ -> False) =<< do
          session1 $ do
            H.tx (Just (H.ReadCommitted, Nothing)) $ do
              ListT.toList . fmap runIdentity =<< do 
                H.streamEx 256 $ [H.stmt|select oid from pg_type ORDER BY oid|]

      it "cursor" $ do
        flip shouldSatisfy isRight =<< do
          session1 $ do
            r :: [(Word, Text)] <-
              H.tx (Just (H.ReadCommitted, Nothing)) $ do
                ListT.toList =<< do H.streamEx 256 $ [H.stmt|select oid, typname from pg_type|]
            r' :: [(Word, Text)] <-
              H.tx (Just (H.ReadCommitted, Nothing)) $ do
                fmap toList $ H.vectorEx $ [H.stmt|select oid, typname from pg_type|]
            liftIO $ (flip shouldBe) r' r

      it "select" $ do
        (flip shouldSatisfy) (either (const False) (not . null)) =<< do
          session1 $ do
            fmap toList $ H.tx Nothing $ H.vectorEx $ 
              [H.stmt|select oid, typname from pg_type|] :: Session [(Word, Text)]

    describe "Mapping of" $ do

      describe "JSON" $ do

        it "encodes and decodes" $ do
          let v = (1, 'a') :: (Int, Char)
              json = J.toJSON v
              in flip shouldBe (Right (Identity json)) =<< do
                   session1 $ H.tx Nothing $ H.singleEx [H.stmt| SELECT ($json :: json) |]

      describe "Enum" $ do

        it "casts text" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ do
              H.tx Nothing $ do
                H.unitEx [H.stmt| DROP TYPE IF EXISTS mood |]
                H.unitEx [H.stmt| CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy') |]
              liftIO . (flip shouldBe) (Just (Identity ("ok" :: Text))) =<< do 
                H.tx Nothing $ H.maybeEx $ [H.stmt|SELECT (? :: mood)|] ("ok" :: Text)

      describe "Unknown" $ do

        it "encodes to enum" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ do
              H.tx Nothing $ do
                H.unitEx [H.stmt| DROP TABLE IF EXISTS a |]
                H.unitEx [H.stmt| DROP TYPE IF EXISTS mood |]
                H.unitEx [H.stmt| CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy') |]
                H.unitEx [H.stmt| CREATE TABLE a (id SERIAL NOT NULL, 
                                             mood mood NOT NULL,
                                             PRIMARY KEY (id)) |]
                H.unitEx $ [H.stmt| INSERT INTO a (mood) VALUES (?) |] (HP.Unknown "ok")
                H.unitEx $ [H.stmt| INSERT INTO a (mood) VALUES (?) |] (HP.Unknown "ok")
                H.unitEx $ [H.stmt| INSERT INTO a (mood) VALUES (?) |] (HP.Unknown "happy")
              liftIO . (flip shouldBe) ([1, 2] :: [Int]) . fmap runIdentity =<< do 
                H.tx Nothing $ fmap toList $ H.vectorEx $ 
                  [H.stmt|SELECT id FROM a WHERE mood = ?|] (HP.Unknown "ok")

        it "encodes Int64 into \"int8\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ H.tx Nothing $ H.unitEx $
              [H.stmt| SELECT (? :: int8) |] 
                (HP.Unknown . PBE.int8 . Left $ 12345)

        it "does not encode Int64 into \"int4\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy (\case Left (H.TxError _) -> True; _ -> False) =<< do
            session1 $ H.tx Nothing $ H.unitEx $
              [H.stmt| SELECT (? :: int4)|] 
                (HP.Unknown . PBE.int8 . Left $ 12345)

        it "encodes Int64 into \"int8\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ H.tx Nothing $ H.unitEx $
              [H.stmt| SELECT (? :: int8) |] 
                (HP.Unknown . PBE.int8 . Left $ 12345)

        it "encodes Day into \"date\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ H.tx Nothing $ H.unitEx $
              [H.stmt| SELECT (? :: date) |] 
                (HP.Unknown . PBE.date $ (read "1900-01-01" :: Day))
              
      describe "Maybe" $ do
        it "" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ do
              validMappingSession (Just '!')
              validMappingSession (Nothing :: Maybe Bool)
      
      describe "List" $ do

        it "1" $ do
          let
            v1  = [v2, v2]
            v2  = [Just 'a', Nothing, Just 'b']
          flip shouldSatisfy isRight =<< do 
            session1 $ do
              validMappingSession v1
              validMappingSession v2

        it "2" $ do
          let
            v1  = [v2, v2]
            v2  = [Just (1 :: Int), Just 2, Nothing]
          flip shouldSatisfy isRight =<< do
            session1 $ do
              validMappingSession v1
              validMappingSession v2

        it "3" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ do
              validMappingSession [" 'a' \"b\" \\c\\ " :: Text]

        it "4" $ do
          let
            v1 =
              [
                [Just 'a', Just 'b'],
                [Nothing, Just 'c']
              ]
          (flip shouldBe) (Right (Just (Identity v1))) =<< do
            session1 $ H.tx Nothing $ do
              H.unitEx $ [H.stmt|DROP TABLE IF EXISTS a|]
              H.unitEx $ [H.stmt|CREATE TABLE a ("v" char[][])|]
              H.unitEx $ [H.stmt|INSERT INTO a (v) VALUES (?)|] v1
              H.maybeEx $ [H.stmt|SELECT v FROM a|]

        it "5" $ do
          flip shouldSatisfy isRight =<< do
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

type Session =
  H.Session HP.Postgres IO

selectSelf :: 
  Backend.CxValue HP.Postgres a => 
  a -> Session (Maybe a)
selectSelf v =
  H.tx Nothing $ (fmap . fmap) runIdentity $ H.maybeEx $ [H.stmt| SELECT ? |] v

session1 :: Session r -> IO (Either (H.SessionError HP.Postgres) r)
session1 =
  session backendSettings poolSettings
  where
    backendSettings = HP.ParamSettings "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ H.poolSettings 6 30

session :: HP.Settings -> H.PoolSettings -> Session r -> IO (Either (H.SessionError HP.Postgres) r)
session s1 s2 m =
  do
    p <- H.acquirePool s1 s2
    r <- H.session p m
    H.releasePool p
    return r

validMappingSession :: 
  Backend.CxValue HP.Postgres a => Typeable a => Show a => Eq a => 
  a -> Session ()
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

mappingProp :: (Show a, Eq a, Backend.CxValue HP.Postgres a) => a -> Property
mappingProp v =
  Right (Just v) === do unsafePerformIO $ session1 $ selectSelf v
