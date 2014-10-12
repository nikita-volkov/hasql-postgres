module HighSQLPostgres.Prelude
( 
  module Exports,
  bug,
  bottom,
  partial,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports

-- mtl-prelude
-------------------------
import MTLPrelude as Exports hiding (shift)

-- mmorph
-------------------------
import Control.Monad.Morph as Exports

-- list-t
-------------------------
import ListT as Exports (ListT)

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- decimal
-------------------------
import Data.Decimal as Exports (Decimal, DecimalRaw)

-- scientific
-------------------------
import Data.Scientific as Exports (Scientific)

-- time
-------------------------
import Data.Time as Exports

-- old-locale
-------------------------
import System.Locale as Exports

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH


bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"high-sql-postgres\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

partial :: Alternative f => (a -> Bool) -> a -> f a
partial p x = 
  if p x then pure x else empty
