module Hasql.Postgres.Connector where

import Hasql.Postgres.Prelude hiding (Error)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


data Settings =
    SettingsParams {
      host :: ByteString,
      port :: Word16,
      user :: Text,
      password :: Text,
      database :: Text
    }
  | SettingsRaw ByteString

data Error =
  BadStatus (Maybe ByteString) |
  UnsupportedVersion Int
  deriving (Show)


-- |
-- Establish and initialize a connection.
open :: Settings -> IO (Either Error PQ.Connection)
open s =
  runEitherT $ do
    c <- lift $ PQ.connectdb (settingsBS s)
    do
      s <- lift $ PQ.status c
      when (s /= PQ.ConnectionOk) $ do
        m <- lift $ PQ.errorMessage c
        left $ BadStatus m
    do
      v <- lift $ PQ.serverVersion c
      when (v < 80200) $ left $ UnsupportedVersion v
    lift $ PQ.exec c $ BL.toStrict $ BB.toLazyByteString $ mconcat $ map (<> BB.char7 ';') $ 
      [ 
        BB.string7 "SET client_encoding = 'UTF8'",
        BB.string7 "SET client_min_messages TO WARNING"
      ]
    return c


settingsBS :: Settings -> ByteString
settingsBS (SettingsRaw r) = r
settingsBS s =
  BL.toStrict $ BB.toLazyByteString $ mconcat $ intersperse (BB.char7 ' ') $ catMaybes $
  [
    mappend (BB.string7 "host=") . BB.byteString <$> 
    partial (not . B.null) (host s)
    ,
    mappend (BB.string7 "port=") . BB.word16Dec <$> 
    partial (/= 0) (port s)
    ,
    mappend (BB.string7 "user=") . BB.byteString . TE.encodeUtf8 <$> 
    partial (not . T.null) (user s)
    ,
    mappend (BB.string7 "password=") . BB.byteString . TE.encodeUtf8 <$> 
    partial (not . T.null) (password s)
    ,
    mappend (BB.string7 "dbname=") . BB.byteString . TE.encodeUtf8 <$> 
    partial (not . T.null) (database s)
  ]

