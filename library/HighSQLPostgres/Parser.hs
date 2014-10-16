module HighSQLPostgres.Parser where

import HighSQLPostgres.Prelude hiding (take)
import Data.Attoparsec.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding


type P = Parser

run :: ByteString -> P a -> Either Text a
run input parser =
  left fromString $ parseOnly (parser <* endOfInput) input


-- ** Parser
-------------------------

labeling :: String -> Parser a -> Parser a
labeling n p = 
  p <?> n

bool :: P Bool
bool =
  labeling "bool" $
    ((string "true" <|> string "True" <|> string "1") *> pure True) <|>
    ((string "false" <|> string "False" <|> string "0") *> pure False)

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
integral :: (Integral a, Num a) => P a
integral =
  do
    n <- negative
    unsignedIntegral >>= return . if n then negate else id
  where
    negative = 
      (charUnit '-' *> pure True) <|> pure False

-- | An unsigned integral value from a sequence of characters.
unsignedIntegral :: (Integral a, Num a) => P a
unsignedIntegral =
  integralDigit >>= fold
  where
    fold a = 
      optional integralDigit >>= maybe (return a) (\b -> fold (a * 10 + b))

-- | An integral value from a single character.
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
    p <- (charUnit '.' *> unsignedIntegral) <|> pure 0
    maybe empty return 
      (makeTimeOfDayValid h m (fromIntegral s + afterPoint (fromIntegral p)))
  where
    afterPoint n = if n < 1 then n else afterPoint (n / 10)

localTime :: P LocalTime
localTime = 
  LocalTime <$> day <*> (charUnit ' ' *> timeOfDay)

timeZone :: P TimeZone
timeZone =
  do
    p <- (charUnit '+' *> pure True) <|> (charUnit '-' *> pure False)
    h <- unsignedIntegral
    m <- (charUnit ':' *> unsignedIntegral) <|> pure 0
    return $!
      minutesToTimeZone ((HighSQLPostgres.Prelude.bool negate id p) (60 * h + m))

zonedTime :: P ZonedTime
zonedTime = 
  ZonedTime <$> localTime <*> timeZone

utcTime :: P UTCTime
utcTime =
  compose <$> day <*> (charUnit ' ' *> timeOfDay) <*> timeZone
  where
    compose day time zone =
      case localToUTCTimeOfDay zone time of
        (dayDelta, time) ->
          UTCTime (addDays dayDelta day) (timeOfDayToTime time)
