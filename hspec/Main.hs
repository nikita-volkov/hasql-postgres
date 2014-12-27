module Main where

import BasePrelude hiding (assert)
import MTLPrelude
import Control.Monad.Trans.Either
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
                tx Nothing $ H.unitTx [H.q|DROP TABLE IF EXISTS a|]
            shouldSatisfy r $ \case
              Left (H.BackendCxError _) -> True
              _ -> False

      it "sameStatementUsedOnDifferentTypes" $ do
        flip shouldSatisfy isRight =<< do
          session1 $ do
            liftIO . (flip shouldBe) (Just (Identity ("abc" :: Text))) =<< do 
              tx Nothing $ H.maybeTx $ [H.q|SELECT ?|] ("abc" :: Text)
            liftIO . (flip shouldBe) (Just (Identity True)) =<< do 
              tx Nothing $ H.maybeTx $ [H.q|SELECT ?|] True

      it "rendering" $ do
        let rows = [("A", 34525), ("B", 324987)] :: [(Text, Int)]
        (flip shouldBe) (Right $ Just $ head rows) =<< do 
          session1 $ do
            tx Nothing $ do
              H.unitTx [H.q|DROP TABLE IF EXISTS a|]
              H.unitTx [H.q|CREATE TABLE a (id SERIAL NOT NULL, 
                                          name VARCHAR NOT NULL, 
                                          birthday INT8,
                                          PRIMARY KEY (id))|]
              forM_ rows $ \(name, birthday) -> do
                H.unitTx $ [H.q|INSERT INTO a (name, birthday) VALUES (?, ?)|] name birthday
            tx Nothing $ do
              H.maybeTx $ [H.q|SELECT name, birthday FROM a WHERE id = ? |] (1 :: Int)

      it "countEffects" $ do
        (flip shouldBe) (Right 100) =<< do 
          session1 $ do
            tx Nothing $ do
              H.unitTx [H.q|DROP TABLE IF EXISTS a|]
              H.unitTx [H.q|CREATE TABLE a (id SERIAL NOT NULL, name VARCHAR NOT NULL)|]
              replicateM_ 100 $ do
                H.unitTx [H.q|INSERT INTO a (name) VALUES ('a')|]
              H.countTx [H.q|DELETE FROM a|]

      it "autoIncrement" $ do
        (flip shouldBe) (Right (Just (1 :: Word64), Just (2 :: Word64))) =<< do
          session1 $ tx Nothing $ do
            H.unitTx [H.q|DROP TABLE IF EXISTS a|]
            H.unitTx [H.q|CREATE TABLE a (id SERIAL NOT NULL, v INT8, PRIMARY KEY (id))|]
            id1 <- (fmap . fmap) runIdentity $ H.maybeTx $ [H.q|INSERT INTO a (v) VALUES (1) RETURNING id|]
            id2 <- (fmap . fmap) runIdentity $ H.maybeTx $ [H.q|INSERT INTO a (v) VALUES (2) RETURNING id|]
            return (id1, id2)

      it "cursorResultsOrder" $ do
        Right r <-
          session1 $ do
            tx (Just (H.ReadCommitted, Nothing)) $ do
              ListT.toList . fmap runIdentity =<< do 
                H.streamTx $ [H.q|select oid from pg_type ORDER BY oid|]
        (flip shouldBe) (sort r :: [Word]) r

      it "cursor" $ do
        flip shouldSatisfy isRight =<< do
          session1 $ do
            r :: [(Word, Text)] <-
              tx (Just (H.ReadCommitted, Nothing)) $ do
                ListT.toList =<< do H.streamTx $ [H.q|select oid, typname from pg_type|]
            r' :: [(Word, Text)] <-
              tx (Just (H.ReadCommitted, Nothing)) $ do
                fmap toList $ H.vectorTx $ [H.q|select oid, typname from pg_type|]
            liftIO $ (flip shouldBe) r' r

      it "select" $ do
        (flip shouldSatisfy) (either (const False) (not . null)) =<< do
          session1 $ do
            fmap toList $ tx Nothing $ H.vectorTx $ 
              [H.q|select oid, typname from pg_type|] :: Session [(Word, Text)]

    describe "Mapping of" $ do

      describe "Enum" $ do

        it "casts text" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ do
              tx Nothing $ do
                H.unitTx [H.q| DROP TYPE IF EXISTS mood |]
                H.unitTx [H.q| CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy') |]
              liftIO . (flip shouldBe) (Just (Identity ("ok" :: Text))) =<< do 
                tx Nothing $ H.maybeTx $ [H.q|SELECT (? :: mood)|] ("ok" :: Text)

      describe "Unknown" $ do

        it "encodes to enum" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ do
              tx Nothing $ do
                H.unitTx [H.q| DROP TABLE IF EXISTS a |]
                H.unitTx [H.q| DROP TYPE IF EXISTS mood |]
                H.unitTx [H.q| CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy') |]
                H.unitTx [H.q| CREATE TABLE a (id SERIAL NOT NULL, 
                                             mood mood NOT NULL,
                                             PRIMARY KEY (id)) |]
                H.unitTx $ [H.q| INSERT INTO a (mood) VALUES (?) |] (HP.Unknown "ok")
                H.unitTx $ [H.q| INSERT INTO a (mood) VALUES (?) |] (HP.Unknown "ok")
                H.unitTx $ [H.q| INSERT INTO a (mood) VALUES (?) |] (HP.Unknown "happy")
              liftIO . (flip shouldBe) ([1, 2] :: [Int]) . fmap runIdentity =<< do 
                tx Nothing $ fmap toList $ H.vectorTx $ 
                  [H.q|SELECT id FROM a WHERE mood = ?|] (HP.Unknown "ok")

        it "encodes Int64 into \"int8\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ tx Nothing $ H.unitTx $
              [H.q| SELECT (? :: int8) |] 
                (HP.Unknown . PBE.int8 . Left $ 12345)

        it "does not encode Int64 into \"int4\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy (\case Left (H.BackendTxError _) -> True; _ -> False) =<< do
            session1 $ tx Nothing $ H.unitTx $
              [H.q| SELECT (? :: int4)|] 
                (HP.Unknown . PBE.int8 . Left $ 12345)

        it "encodes Int64 into \"int8\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ tx Nothing $ H.unitTx $
              [H.q| SELECT (? :: int8) |] 
                (HP.Unknown . PBE.int8 . Left $ 12345)

        it "encodes Day into \"date\" using a \"postgresql-binary\" encoder" $ do
          flip shouldSatisfy isRight =<< do
            session1 $ tx Nothing $ H.unitTx $
              [H.q| SELECT (? :: date) |] 
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
            session1 $ tx Nothing $ do
              H.unitTx $ [H.q|DROP TABLE IF EXISTS a|]
              H.unitTx $ [H.q|CREATE TABLE a ("v" char[][])|]
              H.unitTx $ [H.q|INSERT INTO a (v) VALUES (?)|] v1
              H.maybeTx $ [H.q|SELECT v FROM a|]

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
  ReaderT (H.Pool HP.Postgres) (EitherT (H.TxError HP.Postgres) IO)

tx :: H.TxMode -> (forall s. H.Tx HP.Postgres s a) -> Session a
tx mode m =
  ReaderT $ \p -> EitherT $ H.tx p mode m

selectSelf :: 
  Backend.CxValue HP.Postgres a => 
  a -> Session (Maybe a)
selectSelf v =
  tx Nothing $ (fmap . fmap) runIdentity $ H.maybeTx $ [H.q| SELECT ? |] v

session1 :: Session r -> IO (Either (H.TxError HP.Postgres) r)
session1 =
  session backendSettings poolSettings
  where
    backendSettings = HP.ParamSettings "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ H.poolSettings 6 30

session :: HP.Settings -> H.PoolSettings -> Session r -> IO (Either (H.TxError HP.Postgres) r)
session s1 s2 m =
  do
    p <- H.acquirePool s1 s2
    r <- runEitherT $ runReaderT m p
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
