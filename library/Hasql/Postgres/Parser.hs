module Hasql.Postgres.Parser where

import Hasql.Postgres.Prelude hiding (take, bool)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (double)
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Base16
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Database.PostgreSQL.LibPQ as PQ


type P = Parser

run :: ByteString -> P a -> Either Text a
run input parser =
  left fromString $ parseOnly (parser <* endOfInput) input


-- ** Parser
-------------------------

{-# INLINE labeling #-}
labeling :: String -> Parser a -> Parser a
labeling n p = 
  p <?> n

scientific :: P Scientific
scientific =
  A.scientific

float :: P Float
float =
  realToFrac <$> double

double :: P Double
double = 
  labeling "double" $ A.double

bool :: P Bool
bool =
  labeling "bool" $
    ((string "true" <|> string "t" <|> string "True" <|> string "1") *> pure True) <|>
    ((string "false" <|> string "f" <|> string "False" <|> string "0") *> pure False)

utf8Char :: P Char
utf8Char =
  labeling "utf8Char" $
    asum $ map byLength [1..4]
  where
    byLength l =
      do
        b <- take l
        t <- either (const empty) return $ Data.Text.Encoding.decodeUtf8' b
        (c, _) <- maybe empty return $ Data.Text.uncons t
        return c

utf8LazyText :: P Data.Text.Lazy.Text
utf8LazyText =
  labeling "utf8LazyText" $ do
    b <- takeLazyByteString
    either (const empty) return $ Data.Text.Lazy.Encoding.decodeUtf8' b

utf8Text :: P Text
utf8Text =
  Data.Text.Lazy.toStrict <$> utf8LazyText

charUnit :: Char -> P ()
charUnit c = 
  skip ((==) (fromIntegral (ord c)))

-- | A signed integral value from a sequence of characters.
{-# INLINE integral #-}
integral :: (Integral a, Num a) => P a
integral =
  signed decimal
  
-- | An unsigned integral value from a sequence of characters.
{-# INLINE unsignedIntegral #-}
unsignedIntegral :: (Integral a, Num a) => P a
unsignedIntegral =
  decimal

-- | An integral value from a single character.
{-# INLINE integralDigit #-}
integralDigit :: Integral a => P a
integralDigit = 
  satisfyWith (subtract 48 . fromIntegral) (\n -> n < 10 && n >= 0)

day :: P Day
day =
  do
    y <- unsignedIntegral
    charUnit '-'
    m <- unsignedIntegral
    charUnit '-'
    d <- unsignedIntegral
    maybe empty return (fromGregorianValid y m d)

timeOfDay :: P TimeOfDay
timeOfDay =
  do
    h <- unsignedIntegral
    charUnit ':'
    m <- unsignedIntegral
    charUnit ':'
    s <- unsignedIntegral
    p <- (charUnit '.' *> decimals) <|> pure 0
    maybe empty return 
      (makeTimeOfDayValid h m (fromIntegral s + p))
  where
    decimals = do
      (b, i) <- match unsignedIntegral
      return $ fromIntegral i / (10 ^ Data.ByteString.length b)

localTime :: P LocalTime
localTime = 
  LocalTime <$> day <*> (charUnit ' ' *> timeOfDay)

timeZoneTuple :: P (Bool, Int, Int, Int)
timeZoneTuple =
  do
    p <- (charUnit '+' *> pure True) <|> (charUnit '-' *> pure False)
    h <- unsignedIntegral
    m <- (charUnit ':' *> unsignedIntegral) <|> pure 0
    s <- (charUnit ':' *> unsignedIntegral) <|> pure 0
    return $! (p, h, m, s)

timeZone :: P TimeZone
timeZone =
  do
    (p, h, m, s) <- timeZoneTuple
    return $!
      minutesToTimeZone ((if p then negate else id) (60 * h + m))

-- |
-- Takes seconds in timezone into account.
zonedTime :: P ZonedTime
zonedTime = 
  do
    LocalTime d t <- localTime
    (zp, zh, zm, zs) <- timeZoneTuple
    return $ ZonedTime (LocalTime d (timeOfDayDiffSecs zs t)) (composeTimezone zp zh zm)
  where
    timeOfDayDiffSecs s =
      if s /= 0
        then \t -> timeToTimeOfDay $ timeOfDayToTime t - fromIntegral s
        else id
    composeTimezone p h m =
      minutesToTimeZone ((if p then negate else id) (60 * h + m))

utcTime :: P UTCTime
utcTime =
  UTCTime <$> day <*> (charUnit ' ' *> diffTime)

diffTime :: P DiffTime
diffTime = timeOfDayToTime <$> timeOfDay


-- * Parsable
-------------------------

class Parsable a where
  -- |
  -- @Maybe Char@ indicates which quote to expect for values,
  -- which are quotable.
  parser :: Maybe Char -> P a

instance Parsable a => Parsable (Maybe a) where
  parser q =
    (string "NULL" *> pure Nothing) <|>
    (Just <$> parser q)

instance Parsable a => Parsable [a] where
  parser _ =
    char '{' *> sepBy (parser (Just '"')) (char ',' <* skipSpace) <* char '}'
        
instance Parsable a => Parsable (Vector a) where
  parser _ =
    Data.Vector.fromList <$> parser $bottom

instance Parsable Bool where
  parser =
    \case
      Nothing -> bool
      Just q  -> char q *> bool <* char q

instance Parsable Integer where
  parser = const $ signed decimal

instance Parsable Int where
  parser = const $ signed decimal

instance Parsable Int8 where
  parser = const $ signed decimal

instance Parsable Int16 where
  parser = const $ signed decimal

instance Parsable Int32 where
  parser = const $ signed decimal

instance Parsable Int64 where
  parser = const $ signed decimal

instance Parsable Word where
  parser = const decimal

instance Parsable Word8 where
  parser = const decimal

instance Parsable Word16 where
  parser = const decimal

instance Parsable Word32 where
  parser = const decimal

instance Parsable Word64 where
  parser = const decimal

instance Parsable Float where
  parser = const float

instance Parsable Double where
  parser = const double

instance Parsable Scientific where
  parser = const A.scientific

instance Parsable Day where
  parser =
    \case
      Nothing -> day
      Just q  -> char q *> day <* char q

instance Parsable TimeOfDay where
  parser =
    \case
      Nothing -> timeOfDay
      Just q  -> char q *> timeOfDay <* char q

instance Parsable LocalTime where
  parser =
    \case
      Nothing -> localTime
      Just q  -> char q *> localTime <* char q

instance Parsable ZonedTime where
  parser =
    \case
      Nothing -> zonedTime
      Just q  -> char q *> zonedTime <* char q

instance Parsable UTCTime where
  parser =
    \case
      Nothing -> utcTime
      Just q  -> char q *> utcTime <* char q

instance Parsable Char where
  parser =
    \case
      Nothing -> utf8Char
      Just q  -> utf8Char <|>
                 (char q *> (escapedChar q <|> escapedChar '\\' <|> utf8Char) <* char q)

instance Parsable Text where
  parser =
    \case
      Nothing -> utf8Text
      Just q  -> Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText <$> quotedTextBuilder q

instance Parsable LazyText where
  parser =
    \case
      Nothing -> utf8LazyText
      Just q  -> Data.Text.Lazy.Builder.toLazyText <$> quotedTextBuilder q

instance Parsable ByteString where
  parser =
    \case
      Nothing -> Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString <$>
                 hexByteStringBuilder
      Just q  -> Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString <$>
                 (char q *> hexByteStringBuilder <* char q)

instance Parsable LazyByteString where
  parser =
    \case
      Nothing -> Data.ByteString.Builder.toLazyByteString <$> 
                 hexByteStringBuilder
      Just q  -> Data.ByteString.Builder.toLazyByteString <$> 
                 (char q *> hexByteStringBuilder <* char q)


-- * Unescaping
-------------------------

escapedChar :: Char -> P Char
escapedChar c =
  char '\\' *> char c

unescapedWord8 :: P Word8
unescapedWord8 =
  labeling "unescapedWord8" $ do
    w <- anyWord8
    if w == $([|fromIntegral $ ord '\\'|])
      then anyWord8
      else return w

unescapedUTF8Char :: P Char
unescapedUTF8Char =
  labeling "unescapedUTF8Char" $ 
    let loop attempt b = do
          w <- unescapedWord8
          let b' = b <> Data.ByteString.singleton w
          case Data.Text.Encoding.decodeUtf8' b' of
            Right t -> return $ Data.Text.head t
            Left _ -> 
              if attempt < 4 
                then loop (succ attempt) b'
                else fail "Failed to decode 4 bytes"
        in loop 0 mempty

quotedTextBuilder :: Char -> P Data.Text.Lazy.Builder.Builder
quotedTextBuilder q =
  labeling "quotedTextBuilder" $ 
    char q *> loop
  where
    loop =
      (char q *> pure mempty) <|>
      ((<>) <$> (Data.Text.Lazy.Builder.singleton <$> unescapedUTF8Char) <*> loop)

hexByteStringBuilder :: P Data.ByteString.Builder.Builder
hexByteStringBuilder =
  labeling "hexByteStringBuilder" $ 
    string "\\x" *> loop
  where
    loop =
      ((<>) <$> singleton <*> loop) <|> pure mempty
    singleton = do
      (c, r) <- fmap Data.ByteString.Base16.decode (take 2)
      when (Data.ByteString.length r > 0) $
        fail $ "Invalid hex encoding: " <> show r
      return $ Data.ByteString.Builder.byteString c


