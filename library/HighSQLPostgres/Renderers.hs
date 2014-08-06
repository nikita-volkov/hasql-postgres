-- |
-- Useful info:
-- https://github.com/hdbc/hdbc/blob/7ed3dfad534773cbfe2811ea241d245009e2961b/Database/HDBC/SqlValue.hs#L252
module HighSQLPostgres.Renderers where

import HighSQLPostgres.Prelude
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T


-- | A renderer of @a@.
type R a =
  a -> B.Builder

run :: a -> R a -> ByteString
run a f =
  (L.toStrict . B.toLazyByteString . f) a


-- ** Renderers
-------------------------

-- *** strings
-------------------------

char :: R Char = 
  B.charUtf8

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

int8 :: R Int8 =
  B.int8Dec

int16 :: R Int16 =
  B.int16Dec

int32 :: R Int32 =
  B.int32Dec

int64 :: R Int64 =
  B.int64Dec

integer :: R Integer =
  B.integerDec


-- *** fractionals
-------------------------

float :: R Float =
  B.floatDec
  
double :: R Double =
  B.doubleDec

decimalRawInt32 :: R (DecimalRaw Int32) =
  B.string7 . show

decimalRawInt64 :: R (DecimalRaw Int64) =
  B.string7 . show

decimalRawWord32 :: R (DecimalRaw Word32) =
  B.string7 . show

decimalRawWord64 :: R (DecimalRaw Word64) =
  B.string7 . show

decimal :: R Decimal =
  B.string7 . show

pico :: R Pico =
  B.string7 . showFixed True


-- *** time
-------------------------

day :: R Day = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

timeOfDay :: R TimeOfDay = 
  B.string7 . formatTime defaultTimeLocale "%T%Q"

localTime :: R LocalTime = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))

timeZone :: R TimeZone = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%z"))

zonedTime :: R ZonedTime = 
  \(ZonedTime t z) -> localTime t <> char ' ' <> timeZone z

utcTime :: R UTCTime = 
  B.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))

diffTime :: R DiffTime =
  pico . fromRational . toRational

nominalDiffTime :: R NominalDiffTime =
  pico . fromRational . toRational
