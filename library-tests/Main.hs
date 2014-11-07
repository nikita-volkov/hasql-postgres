{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Hasql.Postgres.Prelude

import {-@ HTF_TESTS @-} Hasql.Postgres.TemplateConverterTests

main = 
  htfMain $ htf_thisModulesTests : htf_importedTests
