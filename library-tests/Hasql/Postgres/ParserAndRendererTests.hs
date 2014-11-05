{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Hasql.Postgres.ParserAndRendererTests where

import Test.Framework
import Test.QuickCheck.Instances
import Hasql.Postgres.Prelude
import qualified Hasql.Postgres.Parser as P
import qualified Hasql.Postgres.Renderer as R


prop_bool :: Bool -> Property =
  isomorphismProperty Nothing

prop_boolSingleQuoted :: Bool -> Property =
  isomorphismProperty (Just '\'')

prop_boolDoubleQuoted :: Bool -> Property =
  isomorphismProperty (Just '"')


prop_integer :: Integer -> Property =
  isomorphismProperty Nothing

prop_integerSingleQuoted :: Integer -> Property =
  isomorphismProperty (Just '\'')

prop_integerDoubleQuoted :: Integer -> Property =
  isomorphismProperty (Just '"')


prop_int :: Int -> Property =
  isomorphismProperty Nothing

prop_intSingleQuoted :: Int -> Property =
  isomorphismProperty (Just '\'')

prop_intDoubleQuoted :: Int -> Property =
  isomorphismProperty (Just '"')


prop_float :: Float -> Property =
  isomorphismProperty Nothing

prop_floatSingleQuoted :: Float -> Property =
  isomorphismProperty (Just '\'')

prop_floatDoubleQuoted :: Float -> Property =
  isomorphismProperty (Just '"')


prop_day :: Day -> Property =
  isomorphismProperty Nothing

prop_daySingleQuoted :: Day -> Property =
  isomorphismProperty (Just '\'')

prop_dayDoubleQuoted :: Day -> Property =
  isomorphismProperty (Just '"')


prop_char :: Char -> Property =
  isomorphismProperty Nothing

prop_charSingleQuoted :: Char -> Property =
  isomorphismProperty (Just '\'')

prop_charDoubleQuoted :: Char -> Property =
  isomorphismProperty (Just '"')


prop_text :: Text -> Property =
  isomorphismProperty Nothing

prop_textSingleQuoted :: Text -> Property =
  isomorphismProperty (Just '\'')

prop_textDoubleQuoted :: Text -> Property =
  isomorphismProperty (Just '"')


isomorphismProperty quotation value =
  Right value === P.run (R.run value (R.renderer quotation)) (P.parser quotation)
