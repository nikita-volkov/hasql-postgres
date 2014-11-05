-- |
-- Useful info:
-- https://github.com/hdbc/hdbc/blob/7ed3dfad534773cbfe2811ea241d245009e2961b/Database/HDBC/SqlValue.hs#L252
module Hasql.Postgres.Renderer where

import Hasql.Postgres.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Base16 as BB16
import qualified Data.ByteString.Base16.Lazy as BB16L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Builder.Scientific
import qualified Data.Vector as Vector


-- | A renderer of @a@.
type R a =
  a -> BB.Builder

run :: a -> R a -> ByteString
run a f =
  (BL.toStrict . BB.toLazyByteString . f) a


-- ** Renderer
-------------------------

ascii :: Show a => R a
ascii =
  BB.string7 . show


-- *** strings
-------------------------

char7 :: R Char = 
  BB.char7

char :: R Char = 
  BB.charUtf8

string7 :: R String = 
  BB.string7

string :: R String = 
  BB.string8

byteString :: R ByteString = 
  BB.byteString

lazyByteString :: R LazyByteString = 
  BB.lazyByteString

text :: R Text =
  T.encodeUtf8Builder

lazyText :: R LazyText =
  TL.encodeUtf8Builder


-- *** enumerations
-------------------------

bool :: R Bool =
  \b -> if b then word8 1 else word8 0

word8 :: R Word8 =
  BB.word8Dec

word16 :: R Word16 =
  BB.word16Dec

word32 :: R Word32 =
  BB.word32Dec

word64 :: R Word64 =
  BB.word64Dec

word :: R Word =
  BB.wordDec

int8 :: R Int8 =
  BB.int8Dec

int16 :: R Int16 =
  BB.int16Dec

int32 :: R Int32 =
  BB.int32Dec

int64 :: R Int64 =
  BB.int64Dec

int :: R Int =
  BB.intDec

integer :: R Integer =
  BB.integerDec

paddedInt :: Int -> R Int
paddedInt padding n =
  if padding <= width
    then int n
    else mconcat (replicate (padding - width) (BB.char7 '0')) <> int n
  where
    width = fromIntegral (succ (floor (logBase 10 (fromIntegral n))) :: Integer)
    

-- *** fractionals
-------------------------

float :: R Float =
  BB.floatDec
  
double :: R Double =
  BB.doubleDec

pico :: R Pico =
  BB.string7 . showFixed True

scientific :: R Scientific =
  Data.ByteString.Builder.Scientific.scientificBuilder


-- *** time
-------------------------

day :: R Day = 
  BB.string7 . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

timeOfDay :: R TimeOfDay = 
  BB.string7 . formatTime defaultTimeLocale "%T%Q"

localTime :: R LocalTime = 
  BB.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))

timeZone :: R TimeZone =
  \(TimeZone t _ _) ->
    if t < 0
      then BB.char7 '-' <> uncurry hm (divMod (negate t) 60)
      else BB.char7 '+' <> uncurry hm (divMod t 60)
  where
    hm h m = 
      paddedInt 2 h <> BB.char7 ':' <> paddedInt 2 m 

zonedTime :: R ZonedTime = 
  \(ZonedTime lt tz) ->
    localTime lt <> timeZone tz

utcTime :: R UTCTime = 
  BB.string7 . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))

diffTime :: R DiffTime =
  pico . fromRational . toRational

nominalDiffTime :: R NominalDiffTime =
  pico . fromRational . toRational


-- * Renderable
-------------------------

class Renderable a where
  -- |
  -- The boolean indicates a quoted mode.
  renderer :: Maybe Char -> R a

instance Renderable a => Renderable (Maybe a) where
  renderer q v =
    maybe (string7 "NULL") (renderer q) v

instance Renderable a => Renderable [a] where
  renderer _ v =
    execWriter $ do
      tell $ char7 '{'
      tell $ mconcat $ intersperse (string7 ", ") $ map (renderer (Just '"')) v
      tell $ char7 '}'

instance Renderable a => Renderable (Vector a) where
  renderer _ =
    renderer $bottom . Vector.toList

instance Renderable Bool where
  renderer q =
    maybe id quoting q . \case
      True -> char7 't'
      False -> char7 'f'

instance Renderable Int where
  renderer _ = 
    BB.intDec

instance Renderable Int8 where
  renderer _ = 
    BB.int8Dec

instance Renderable Int16 where
  renderer _ = 
    BB.int16Dec

instance Renderable Int32 where
  renderer _ = 
    BB.int32Dec

instance Renderable Int64 where
  renderer _ = 
    BB.int64Dec

instance Renderable Integer where
  renderer _ = 
    BB.integerDec

instance Renderable Word where
  renderer _ = 
    BB.wordDec

instance Renderable Word8 where
  renderer _ = 
    BB.word8Dec

instance Renderable Word16 where
  renderer _ = 
    BB.word16Dec

instance Renderable Word32 where
  renderer _ = 
    BB.word32Dec

instance Renderable Word64 where
  renderer _ = 
    BB.word64Dec

instance Renderable Float where
  renderer _ = 
    BB.floatDec

instance Renderable Double where
  renderer _ = 
    BB.doubleDec

instance Renderable Scientific where
  renderer _ = 
    Data.ByteString.Builder.Scientific.scientificBuilder

instance Renderable Day where
  renderer q = 
    maybe id quoting q . day

instance Renderable TimeOfDay where
  renderer q = 
    maybe id quoting q . timeOfDay

instance Renderable LocalTime where
  renderer q = 
    maybe id quoting q . localTime

instance Renderable ZonedTime where
  renderer q = 
    maybe id quoting q . zonedTime

instance Renderable UTCTime where
  renderer q = 
    maybe id quoting q . utcTime

instance Renderable Char where
  renderer q =
    renderer Nothing . T.singleton

instance Renderable Text where
  renderer =
    maybe T.encodeUtf8Builder (\q -> quoting q . escaping q . T.encodeUtf8)
    where
      escaping q =
        BC.foldr (mappend . escapedChar8 q) mempty

instance Renderable LazyText where
  renderer q =
    maybe TL.encodeUtf8Builder (\qc -> quoting qc . escaping qc . TL.encodeUtf8) q
    where
      escaping q =
        BLC.foldr (mappend . escapedChar8 q) mempty

instance Renderable ByteString where
  renderer =
    (. encode) . maybe (BB.string7 "\\x" <>) (\q -> quoting q . (BB.string7 "\\\\x" <>))
    where
      encode = BB.byteString . BB16.encode

instance Renderable LazyByteString where
  renderer =
    (. encode) . maybe (BB.string7 "\\x" <>) (\q -> quoting q . (BB.string7 "\\\\x" <>))
    where
      encode = BB.lazyByteString . BB16L.encode


-- ** Helpers
-------------------------

quoting :: Char -> R BB.Builder
quoting q =
  (char7 q <>) . (<> char7 q)

escapedChar8 :: Char -> R Char
escapedChar8 q c =
  (if c == q || c == '\\' then (char7 '\\' <>) else id) $ BB.char8 c

