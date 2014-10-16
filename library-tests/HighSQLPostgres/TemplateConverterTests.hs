{-# OPTIONS_GHC -F -pgmF htfpp #-}
module HighSQLPostgres.TemplateConverterTests where

import Test.Framework
import HighSQLPostgres.Prelude
import HighSQLPostgres.Parser
import qualified Data.Text.Encoding
import qualified HighSQLPostgres.TemplateConverter


test_convert =
  assertEqual
    (Right "asdf $1 \"'\\\"?'\" d 2$2$3")
    (HighSQLPostgres.TemplateConverter.convert "asdf ? \"'\\\"?'\" d 2??")
  

