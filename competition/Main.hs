import BasePrelude
import MTLPrelude
import Control.DeepSeq
import Control.Monad.Trans.Control
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Time
import CriterionPlus
import qualified Hasql as H
import qualified Hasql.Postgres as H
import qualified ListT
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.SqlQQ as P
import qualified Database.PostgreSQL.Simple.FromField as P
import qualified Database.PostgreSQL.Simple.ToField as P
import qualified Database.PostgreSQL.Simple.FromRow as P
import qualified Database.PostgreSQL.Simple.ToRow as P
import qualified Database.PostgreSQL.Simple.Transaction as P
import qualified Database.HDBC as C
import qualified Database.HDBC.PostgreSQL as C
import qualified Test.QuickCheck.Gen as Q
import qualified Test.QuickCheck.Arbitrary as Q
import qualified Test.QuickCheck.Instances


main =
  benchmark $ do

    standoff "Results parsing" $ do

      rows :: [(Text, Day)] <- 
        liftIO $ replicateM 100 $ 
          (,) <$> Q.generate Q.arbitrary <*> Q.generate Q.arbitrary

      subject "hasql" $ do
        pause
        H.session (H.ParamSettings host port user password db) (fromJust $ H.sessionSettings 1 30) $ do
          H.tx Nothing $ do
            H.unit [H.q|DROP TABLE IF EXISTS a|]
            H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, 
                                        name VARCHAR NOT NULL, 
                                        birthday DATE,
                                        PRIMARY KEY (id))|]
            forM_ rows $ \(name, birthday) -> do
              H.unit $ [H.q|INSERT INTO a (name, birthday) VALUES (?, ?)|] name birthday
          lift $ continue
          replicateM_ 100 $ do
            r <- H.tx Nothing $ do
              H.list $ [H.q|SELECT * FROM a|] :: H.Tx H.Postgres s [(Int, Text, Day)]
            deepseq r $ return ()
          lift $ pause

      subject "postgresql-simple" $ do
        pause
        c <- liftIO $ P.connect $ P.ConnectInfo host port user password db
        liftIO $ P.execute_ c "SET client_min_messages TO WARNING"
        liftIO $ P.execute_ c "DROP TABLE IF EXISTS a"
        liftIO $ P.execute_ c [P.sql|CREATE TABLE a (id SERIAL NOT NULL, 
                                                     name VARCHAR NOT NULL, 
                                                     birthday DATE, 
                                                     PRIMARY KEY (id))|]
        forM_ rows $ \(name, birthday) -> do
          liftIO $ P.execute c "INSERT INTO a (name, birthday) VALUES (?, ?)" (name, birthday)
        continue
        liftIO $ replicateM_ 100 $ do
          r <- P.query_ c "SELECT * FROM a" :: IO [(Int, Text, Day)]
          deepseq r $ return ()
        pause
        liftIO $ P.close c

      subject "HDBC" $ do
        pause
        c <- liftIO $ C.connectPostgreSQL $ concat $ intersperse " " $
               [
                 "host=" <> host,
                 "port=" <> show port,
                 "user=" <> user,
                 "password=" <> password,
                 "dbname=" <> db
               ]
        liftIO $ C.run c "SET client_min_messages TO WARNING" []
        liftIO $ C.run c "DROP TABLE IF EXISTS a" []
        liftIO $ C.run c 
          "CREATE TABLE a (id SERIAL NOT NULL, \
                           \name VARCHAR NOT NULL, \
                           \birthday DATE, \
                           \PRIMARY KEY (id))"
          []
        forM_ rows $ \(name, birthday) -> do
          liftIO $ C.run c "INSERT INTO a (name, birthday) VALUES (?, ?)" [C.toSql name, C.toSql birthday]
        liftIO $ C.commit c
        continue
        liftIO $ replicateM_ 100 $ do
          r <- do
            r <- C.quickQuery c "SELECT * FROM a" []
            return $ flip map r $ \[id, name, birthday] ->
              (C.fromSql id :: Int, C.fromSql name :: Text, C.fromSql birthday :: Day) 
          deepseq r $ return ()
        pause
        liftIO $ C.disconnect c


    standoff "Templates and rendering" $ do

      rows :: [(Text, Day)] <- 
        liftIO $ Q.generate $ replicateM 100 $ 
          (,) <$> Q.arbitrary <*> Q.arbitrary
          
      let !day = read "2014-10-26" :: Day

      subject "hasql" $ do
        pause
        H.session (H.ParamSettings host port user password db) (fromJust $ H.sessionSettings 1 30) $ do
          H.tx Nothing $ do
            H.unit [H.q|DROP TABLE IF EXISTS a|]
            H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, 
                                        name VARCHAR NOT NULL, 
                                        birthday DATE,
                                        PRIMARY KEY (id))|]
          lift $ continue
          replicateM_ 1000 $ do
            H.tx Nothing $ do
              H.list $ 
                [H.q|SELECT * FROM a WHERE id > ? AND id < ? AND birthday != ?|] 
                  (1000 :: Int)
                  (0 :: Int) 
                  (day)
                :: H.Tx H.Postgres s [(Int, Text, Day)]
          lift $ pause

      subject "postgresql-simple" $ do
        pause
        c <- liftIO $ P.connect $ P.ConnectInfo host port user password db
        liftIO $ P.execute_ c "SET client_min_messages TO WARNING"
        liftIO $ P.execute_ c "DROP TABLE IF EXISTS a"
        liftIO $ P.execute_ c [P.sql|CREATE TABLE a (id SERIAL NOT NULL, 
                                                     name VARCHAR NOT NULL, 
                                                     birthday DATE, 
                                                     PRIMARY KEY (id))|]
        continue
        liftIO $ replicateM_ 1000 $ do
          P.query c "SELECT * FROM a WHERE id > ? AND id < ? AND birthday != ?" 
            (1000 :: Int, 0 :: Int, day) 
            :: IO [(Int, Text, Day)]
        pause
        liftIO $ P.close c

      subject "HDBC" $ do
        pause
        c <- liftIO $ C.connectPostgreSQL $ concat $ intersperse " " $
               [
                 "host=" <> host,
                 "port=" <> show port,
                 "user=" <> user,
                 "password=" <> password,
                 "dbname=" <> db
               ]
        liftIO $ C.run c "SET client_min_messages TO WARNING" []
        liftIO $ C.run c "DROP TABLE IF EXISTS a" []
        liftIO $ C.run c 
          "CREATE TABLE a (id SERIAL NOT NULL, \
                           \name VARCHAR NOT NULL, \
                           \birthday DATE, \
                           \PRIMARY KEY (id))"
          []
        liftIO $ C.commit c
        continue
        liftIO $ replicateM_ 1000 $ do
          C.quickQuery c "SELECT * FROM a WHERE id > ? AND id < ? AND birthday != ?" 
                         [C.toSql (1000 :: Int), C.toSql (0 :: Int), C.toSql (day)]
        pause
        liftIO $ C.disconnect c


    standoff "Writing transaction" $ do
      
      let users = 20 :: Int
          transfers = do
            a <- [1..users]
            b <- [1..users]
            if a /= b then return (a, b) else mzero
          amount = 3 :: Int64

      subject "hasql" $ do
        pause
        H.session (H.ParamSettings host port user password db) (fromJust $ H.sessionSettings 1 30) $ do
          H.tx Nothing $ do
            H.unit [H.q|DROP TABLE IF EXISTS a|]
            H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))|]
            replicateM_ users $ do
              H.unit [H.q|INSERT INTO a (balance) VALUES (0)|]
          lift $ continue
          forM_ transfers $ \(id1, id2) -> do
            H.tx (Just (H.Serializable, True)) $ do
              runMaybeT $ do
                do
                  Identity balance <- MaybeT $ H.single $ [H.q|SELECT balance FROM a WHERE id=?|] id1
                  lift $ H.unit $ [H.q|UPDATE a SET balance=? WHERE id=?|] (balance - amount) id1
                do
                  Identity balance <- MaybeT $ H.single $ [H.q|SELECT balance FROM a WHERE id=?|] id2
                  lift $ H.unit $ [H.q|UPDATE a SET balance=? WHERE id=?|] (balance + amount) id2
          lift $ pause

      subject "postgresql-simple" $ do
        pause
        c <- liftIO $ P.connect $ P.ConnectInfo host port user password db
        liftIO $ P.execute_ c "SET client_min_messages TO WARNING"
        liftIO $ P.execute_ c "DROP TABLE IF EXISTS a"
        liftIO $ P.execute_ c "CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))"
        replicateM_ users $ do
          liftIO $ P.execute_ c "INSERT INTO a (balance) VALUES (0)"
        continue
        liftIO $ forM_ transfers $ \(id1, id2) -> do
          P.withTransactionMode (P.TransactionMode P.Serializable P.ReadWrite) c $ do
            do
              [P.Only balance] <- P.query c "SELECT balance FROM a WHERE id=?" (P.Only id1)
              P.execute c "UPDATE a SET balance=? WHERE id=?" ((balance - amount), id1)
            do
              [P.Only balance] <- P.query c "SELECT balance FROM a WHERE id=?" (P.Only id2)
              P.execute c "UPDATE a SET balance=? WHERE id=?" ((balance + amount), id1)
        pause
        liftIO $ P.close c

      subject "HDBC" $ do
        pause
        c <- liftIO $ C.connectPostgreSQL $ concat $ intersperse " " $
               [
                 "host=" <> host,
                 "port=" <> show port,
                 "user=" <> user,
                 "password=" <> password,
                 "dbname=" <> db
               ]
        liftIO $ C.run c "SET client_min_messages TO WARNING" []
        liftIO $ C.run c "DROP TABLE IF EXISTS a" []
        liftIO $ C.run c "CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))" []
        liftIO $ replicateM_ users $ do
          C.run c "INSERT INTO a (balance) VALUES (0)" []
        liftIO $ C.commit c
        continue
        liftIO $ forM_ transfers $ \(id1, id2) -> do
          C.withTransaction c $ \c -> do
            [[balance1]] <- C.quickQuery c "SELECT balance FROM a WHERE id=?" [C.toSql id1]
            [[balance2]] <- C.quickQuery c "SELECT balance FROM a WHERE id=?" [C.toSql id2]
            C.run c "UPDATE a SET balance=? WHERE id=?" [C.toSql (C.fromSql balance1 - amount), C.toSql id1]
            C.run c "UPDATE a SET balance=? WHERE id=?" [C.toSql (C.fromSql balance2 + amount), C.toSql id2]
        pause
        liftIO $ C.disconnect c



host = "localhost"
port = 5432
user = "postgres"
password = ""
db = "postgres"
