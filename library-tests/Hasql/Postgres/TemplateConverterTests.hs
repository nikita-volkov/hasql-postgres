{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Hasql.Postgres.TemplateConverterTests where

import Test.Framework
import Hasql.Postgres.Prelude
import qualified Data.Text.Encoding
import qualified Hasql.Postgres.TemplateConverter


test_convert =
  assertEqual
    (Right "asdf $1 \"'\\\"?'\" d 2$2$3")
    (Hasql.Postgres.TemplateConverter.convert "asdf ? \"'\\\"?'\" d 2??")
  

