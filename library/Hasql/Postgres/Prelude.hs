module Hasql.Postgres.Prelude
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

-- scientific
-------------------------
import Data.Scientific as Exports (Scientific)

-- time
-------------------------
import Data.Time as Exports

-- old-locale
-------------------------
import System.Locale as Exports

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH


bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"hasql-postgres\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

partial :: Alternative f => (a -> Bool) -> a -> f a
partial p x = 
  if p x then pure x else empty
