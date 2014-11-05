module Hasql.Postgres.OIDMapping where

import Hasql.Postgres.Prelude hiding (bool)
import Database.PostgreSQL.LibPQ (Oid(..))
import qualified Language.Haskell.TH as TH
import qualified Hasql.Postgres.PTI as PTI


class OIDMapping a where
  identifyOID :: a -> Oid
  identifyArrayOID :: a -> Oid

instance OIDMapping a => OIDMapping (Maybe a) where
  identifyOID = const $ identifyOID (undefined :: a)
  identifyArrayOID = const $ identifyArrayOID (undefined :: a)

instance OIDMapping a => OIDMapping (Vector a) where
  identifyOID = const $ identifyArrayOID (undefined :: a)
  identifyArrayOID = identifyOID

let
  mappings =
    [ (''Bool          , PTI.bool       ),
      (''Int           , PTI.int8       ),
      (''Int8          , PTI.int2       ),
      (''Int16         , PTI.int2       ),
      (''Int32         , PTI.int4       ),
      (''Int64         , PTI.int8       ),
      (''Word          , PTI.int8       ),
      (''Word8         , PTI.int2       ),
      (''Word16        , PTI.int4       ),
      (''Word32        , PTI.int8       ),
      (''Word64        , PTI.int8       ),
      (''Float         , PTI.float4     ),
      (''Double        , PTI.float8     ),
      (''Scientific    , PTI.numeric    ),
      (''Day           , PTI.date       ),
      (''TimeOfDay     , PTI.time       ),
      (''LocalTime     , PTI.timestamp  ),
      (''ZonedTime     , PTI.timestamptz),
      (''UTCTime       , PTI.timestamp  ),
      (''Char          , PTI.varchar    ),
      (''Text          , PTI.text       ),
      (''LazyText      , PTI.text       ),
      (''ByteString    , PTI.bytea      ),
      (''LazyByteString, PTI.bytea      ) ]
  oidInt (Oid x) = 
    fromIntegral x
  in
    fmap join $ forM mappings $ \(t, pti) ->
      [d|
        instance OIDMapping $(TH.conT t) where
          {-# INLINE identifyOID #-}
          identifyOID = 
            const $ Oid $(TH.litE $ TH.integerL $ oidInt $ PTI.oidOf pti)
          {-# INLINE identifyArrayOID #-}
          identifyArrayOID =
            const $ Oid $(TH.litE $ TH.integerL $ oidInt $ fromMaybe (PTI.oidOf PTI.unknown) (PTI.arrayOIDOf pti))
      |]
