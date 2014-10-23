import BasePrelude
import MTLPrelude
import Data.Text (Text)
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
import qualified Test.QuickCheck.Gen as Q
import qualified Test.QuickCheck.Arbitrary as Q
import qualified Test.QuickCheck.Instances


main =
  benchmark $ do

    standoff "Transaction" $ do
      
      let users = 20 :: Int
          transfers = do
            a <- [1..users]
            b <- [1..users]
            if a /= b then return (a, b) else mzero
          amount = 3 :: Int64

      subject "hasql" $ do
        pause
        H.session (H.Postgres host port user password db) (fromJust $ H.sessionSettings 1 30) $ do
          H.tx Nothing $ do
            H.unit [H.q|DROP TABLE IF EXISTS a|]
            H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))|]
            replicateM_ users $ do
              H.unit [H.q|INSERT INTO a (balance) VALUES (0)|]
          lift $ continue
          forM_ transfers $ \(id1, id2) -> do
            H.tx (Just (H.Serializable, True)) $ do
              ListT.head $ do
                do
                  Identity balance <- H.stream False $ [H.q|SELECT balance FROM a WHERE id=?|] id1
                  lift $ H.unit $ [H.q|UPDATE a SET balance=? WHERE id=?|] (balance - amount) id1
                do
                  Identity balance <- H.stream False $ [H.q|SELECT balance FROM a WHERE id=?|] id2
                  lift $ H.unit $ [H.q|UPDATE a SET balance=? WHERE id=?|] (balance + amount) id2
          lift $ pause

      subject "postgresql-simple" $ do
        pause
        c <- liftIO $ P.connect $ P.ConnectInfo host port user password db
        liftIO $ P.execute_ c "DROP TABLE IF EXISTS a"
        liftIO $ P.execute_ c "CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))"
        replicateM_ users $ do
          liftIO $ P.execute_ c "INSERT INTO a (balance) VALUES (0)"
        continue
        forM_ transfers $ \(id1, id2) -> do
          liftIO $ P.withTransactionMode (P.TransactionMode P.Serializable P.ReadWrite) c $ do
            do
              [P.Only balance] <- P.query c "SELECT balance FROM a WHERE id=?" (P.Only id1)
              P.execute c "UPDATE a SET balance=? WHERE id=?" ((balance - amount), id1)
            do
              [P.Only balance] <- P.query c "SELECT balance FROM a WHERE id=?" (P.Only id2)
              P.execute c "UPDATE a SET balance=? WHERE id=?" ((balance + amount), id1)
        pause
        liftIO $ P.close c

    standoff "Query" $ do

      rows :: [(Text, Day)] <- 
        liftIO $ replicateM 100 $ 
          (,) <$> Q.generate Q.arbitrary <*> Q.generate Q.arbitrary

      subject "hasql" $ do
        pause
        H.session (H.Postgres host port user password db) (fromJust $ H.sessionSettings 1 30) $ do
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
            H.tx Nothing $ do
              ListT.toList $ H.stream False $ [H.q|SELECT * FROM a|] :: H.Tx H.Postgres s [(Int, Text, Day)]
          lift $ pause

      subject "postgresql-simple" $ do
        pause
        c <- liftIO $ P.connect $ P.ConnectInfo host port user password db
        liftIO $ P.execute_ c "DROP TABLE IF EXISTS a"
        liftIO $ P.execute_ c [P.sql|CREATE TABLE a (id SERIAL NOT NULL, 
                                                     name VARCHAR NOT NULL, 
                                                     birthday DATE, 
                                                     PRIMARY KEY (id))|]
        forM_ rows $ \(name, birthday) -> do
          liftIO $ P.execute c "INSERT INTO a (name, birthday) VALUES (?, ?)" (name, birthday)
        continue
        liftIO $ replicateM_ 100 $ do
          P.query_ c "SELECT * FROM a" :: IO [(Int, Text, Day)]
        pause
        liftIO $ P.close c



host = "localhost"
port = 5432
user = "postgres"
password = ""
db = "postgres"
