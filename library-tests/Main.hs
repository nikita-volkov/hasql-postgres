{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import HighSQLPostgres.Prelude

import {-@ HTF_TESTS @-} HighSQLPostgres.ParserTests
import {-@ HTF_TESTS @-} HighSQLPostgres.TemplateConverterTests

main = 
  htfMain $ htf_thisModulesTests : htf_importedTests
