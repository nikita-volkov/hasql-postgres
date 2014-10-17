{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Hasql.Postgres.ParserTests where

import Test.Framework
import Hasql.Postgres.Prelude
import Hasql.Postgres.Parser
import qualified Data.Text.Encoding


prop_utf8CharBackAndForth c =
  let
    bs = Data.Text.Encoding.encodeUtf8 $ fromString [c]
    in Right c === run bs utf8Char

