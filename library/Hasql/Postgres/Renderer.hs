-- |
-- Useful info:
-- https://github.com/hdbc/hdbc/blob/7ed3dfad534773cbfe2811ea241d245009e2961b/Database/HDBC/SqlValue.hs#L252
module Hasql.Postgres.Renderer where

import Hasql.Postgres.Prelude
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder.Scientific


-- | A renderer of @a@.
type R a =
  a -> B.Builder

run :: a -> R a -> ByteString
run a f =
  (L.toStrict . B.toLazyByteString . f) a


-- ** Renderer
-------------------------

ascii :: Show a => R a
ascii =
  B.string7 . show


-- *** strings
-------------------------

char7 :: R Char = 
  B.char7

char :: R Char = 
  B.charUtf8

string7 :: R String = 
  B.string7

string :: R String = 
  B.string8

byteString :: R ByteString = 
  B.byteString

text :: R Text =
  byteString . T.encodeUtf8


-- *** enumerations
-------------------------

bool :: R Bool =
  \b -> if b then word8 1 else word8 0

word8 :: R Word8 =
  B.word8Dec

word16 :: R Word16 =
  B.word16Dec

word32 :: R Word32 =
  B.word32Dec

word64 :: R Word64 =
  B.word64Dec

word :: R Word =
  B.wordDec

int8 :: R Int8 =
  B.int8Dec

int16 :: R Int16 =
  B.int16Dec

int32 :: R Int32 =
  B.int32Dec

int64 :: R Int64 =
  B.int64Dec

int :: R Int =
  B.intDec

integer :: R Integer =
  B.integerDec

paddedInt :: Int -> R Int
paddedInt padding n =
  if padding <= width
    then int n
    else mconcat (replicate (padding - width) (B.char7 '0')) <> int n
  where
    width = fromIntegral (succ (floor (logBase 10 (fromIntegral n))) :: Integer)
    

-- *** fractionals
-------------------------

float :: R Float =
  B.floatDec
  
double :: R Double =
  B.doubleDec

decimalRawInt32 :: R (DecimalRaw Int32) =
  ascii

decimalRawInt64 :: R (DecimalRaw Int64) =
  ascii

decimalRawWord32 :: R (DecimalRaw Word32) =
  ascii

decimalRawWord64 :: R (DecimalRaw Word64) =
  ascii

decimal :: R Decimal =
  ascii

pico :: R Pico =
  B.string7 . showFixed True

scientific :: R Scientific =
  Data.ByteString.Builder.Scientific.scientificBuilder

-- *** time
-------------------------

day :: R Day = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

timeOfDay :: R TimeOfDay = 
  B.string7 . formatTime defaultTimeLocale "%T%Q"

localTime :: R LocalTime = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))

timeZone :: R TimeZone =
  \(TimeZone t _ _) ->
    if t < 0
      then B.char7 '-' <> uncurry hm (divMod (negate t) 60)
      else B.char7 '+' <> uncurry hm (divMod t 60)
  where
    hm h m = 
      paddedInt 2 h <> B.char7 ':' <> paddedInt 2 m 

zonedTime :: R ZonedTime = 
  \(ZonedTime lt tz) ->
    localTime lt <> timeZone tz

utcTime :: R UTCTime = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))

diffTime :: R DiffTime =
  pico . fromRational . toRational

nominalDiffTime :: R NominalDiffTime =
  pico . fromRational . toRational
