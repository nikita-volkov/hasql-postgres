{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HighSQLPostgres.ParserTests where

import Test.Framework
import HighSQLPostgres.Prelude
import HighSQLPostgres.Parser
import qualified Data.Text.Encoding


prop_utf8CharBackAndForth c =
  let
    bs = Data.Text.Encoding.encodeUtf8 $ fromString [c]
    in Right c === run bs utf8Char

