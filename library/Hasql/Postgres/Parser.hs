module Hasql.Postgres.Parser where

import Hasql.Postgres.Prelude hiding (take)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (double)
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Attoparsec.ByteString.Char8 as A


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
      minutesToTimeZone ((Hasql.Postgres.Prelude.bool negate id p) (60 * h + m))

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
      minutesToTimeZone ((Hasql.Postgres.Prelude.bool negate id p) (60 * h + m))

utcTime :: P UTCTime
utcTime =
  UTCTime <$> day <*> (charUnit ' ' *> diffTime)

diffTime :: P DiffTime
diffTime = timeOfDayToTime <$> timeOfDay
