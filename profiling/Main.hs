import BasePrelude
import MTLPrelude
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Time
import qualified Hasql as H
import qualified Hasql.Postgres as H
import qualified ListT
import qualified Test.QuickCheck.Gen as Q
import qualified Test.QuickCheck.Arbitrary as Q
import qualified Test.QuickCheck.Instances


main = do
  rows :: [(Text, Day)] <- 
    liftIO $ replicateM 100 $ 
      (,) <$> Q.generate Q.arbitrary <*> Q.generate Q.arbitrary
  H.session (H.Postgres host port user password db) (fromJust $ H.sessionSettings 1 30) $ do
    H.tx Nothing $ do
      H.unit [H.q|DROP TABLE IF EXISTS a|]
      H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, 
                                  name VARCHAR NOT NULL, 
                                  birthday DATE, 
                                  PRIMARY KEY (id))|]
      forM_ rows $ \(name, birthday) -> do
        H.unit $ [H.q|INSERT INTO a (name, birthday) VALUES (?, ?)|] name birthday
    replicateM_ 2000 $ do
      H.tx Nothing $ do
        H.list $ [H.q|SELECT * FROM a|] :: H.Tx H.Postgres s [(Int, Text, Day)]


host = "localhost"
port = 5432
user = "postgres"
password = ""
db = "postgres"
