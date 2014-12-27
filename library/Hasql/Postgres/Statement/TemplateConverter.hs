module Hasql.Postgres.Statement.TemplateConverter where

import Hasql.Postgres.Prelude
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Hasql.Postgres.Statement.TemplateConverter.Parser as Parser


-- |
-- 
-- >>> BB.toLazyByteString $ convert "asdf ? \"'\\\"?'\" d 2??"
-- "asdf $1 \"'\\\"?'\" d 2$2$3"
convert :: Text -> BB.Builder
convert template =
  either ($bug . showString "Unparsable template: " . shows template . 
          showString "; Error: " . show) id $ 
  do
    parts <- Parser.run (TE.encodeUtf8 template) Parser.parts
    return $
      mconcat $ ($ 1) $ evalState $ do
        forM parts $ \case
          Parser.Chunk c -> do
            return c
          Parser.Placeholder -> do
            i <- get
            put $ succ i
            return $ BB.char8 '$' <> BB.wordDec i
