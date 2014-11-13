module Hasql.Postgres.Prelude
( 
  module Exports,
  LazyByteString,
  LazyText,
  bug,
  bottom,
  partial,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right)

-- mtl-prelude
-------------------------
import MTLPrelude as Exports hiding (shift)

-- either
-------------------------
import Control.Monad.Trans.Either as Exports

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

-- uuid
-------------------------
import Data.UUID as Exports (UUID)

-- time
-------------------------
import Data.Time as Exports

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- loch-th
-------------------------
import Debug.Trace.LocationTH as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH
import qualified Data.Text.Lazy
import qualified Data.ByteString.Lazy


type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"hasql-postgres\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

partial :: Alternative f => (a -> Bool) -> a -> f a
partial p x = 
  if p x then pure x else empty
