module HighSQLPostgres.TemplateConverter.Parser where

import HighSQLPostgres.Prelude
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB


data Part =
  Chunk BB.Builder |
  Placeholder

run :: ByteString -> Parser a -> Either Text a
run input parser =
  left fromString $ parseOnly (parser <* endOfInput) input

parts :: Parser [Part]
parts =
  many (chunk <|> placeholder)
  where
    chunk = 
      fmap Chunk $ fmap mconcat $ many1 $ stringLit <|> (BB.char8 <$> notChar '?')
    placeholder = 
      char '?' *> pure Placeholder

stringLit :: Parser BB.Builder
stringLit =
  do
    quote <- 
      char '"' <|> char '\''
    contentBuilders <- 
      many $ 
        (BB.byteString <$> string "\\\\") <|> 
        (BB.byteString <$> string (fromString ['\\', quote])) <|> 
        (BB.char8 <$> notChar quote)
    char quote
    return $
      BB.char7 quote <> mconcat contentBuilders <> BB.char7 quote

