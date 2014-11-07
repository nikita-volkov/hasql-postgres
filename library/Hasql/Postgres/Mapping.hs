-- |
-- Internal mappings.
module Hasql.Postgres.Mapping where

import Hasql.Postgres.Prelude hiding (bool)
import qualified Language.Haskell.TH as TH
import qualified Hasql.Postgres.PTI as PTI
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Encoder as Encoder
import qualified PostgreSQLBinary.Decoder as Decoder


type Value =
  Maybe ByteString

-- |
-- A final value level mapping.
class Mapping a where
  oid :: a -> PTI.OID
  encode :: a -> Value
  decode :: Value -> Either Text a

-- |
-- A mapping for construction of array values.
class ArrayMapping a where
  arrayOID :: a -> PTI.OID
  arrayEncode :: a -> Array.Data
  arrayDecode :: Array.Data -> Either Text a


instance Mapping a => Mapping (Maybe a) where
  oid = 
    const $ oid (undefined :: a)
  encode = 
    join . traverse encode
  decode = 
    maybe (return Nothing) (fmap Just . decode . Just)

instance (ArrayMapping a, Mapping a) => ArrayMapping (Maybe a) where
  arrayOID =
    const $ arrayOID (undefined :: a)
  arrayEncode =
    \case
      Nothing -> 
        Array.fromSingleton Nothing True (PTI.oidWord32 (oid (undefined :: a)))
      Just x ->
        setNullable True $ arrayEncode x
        where
          setNullable x (dl, vl, _, oid) = (dl, vl, True, oid)
  arrayDecode =
    \case
      (_, [x], _, _) -> decode x
      x -> Left $ "Array data doesn't match the 'Maybe' type: " <> (fromString . show) x


instance (Mapping a, ArrayMapping a) => Mapping [a] where
  oid = 
    arrayOID
  encode = 
    Just . Encoder.array . arrayEncode
  decode x = 
    do
      b <- maybe (Left "NULL input") return x
      a <- Decoder.array b
      arrayDecode a

instance (Mapping a, ArrayMapping a) => ArrayMapping [a] where
  arrayOID =
    const $ arrayOID (undefined :: a)
  arrayEncode =
    \case
      [] -> ([(0, 1)], [], False, PTI.oidWord32 $ oid (undefined :: a))
      x -> Array.fromListUnsafe . map arrayEncode $ x
  arrayDecode x =
    traverse arrayDecode $ Array.elements x


instance (Mapping a, ArrayMapping a) => Mapping (Vector a) where
  oid =
    arrayOID
  encode = 
    Just . Encoder.array . arrayEncode
  decode x = 
    do
      b <- maybe (Left "NULL input") return x
      a <- Decoder.array b
      arrayDecode a

instance (Mapping a, ArrayMapping a) => ArrayMapping (Vector a) where
  arrayOID =
    const $ arrayOID (undefined :: a)
  arrayEncode =
    arrayEncode . V.toList
  arrayDecode =
    fmap V.fromList . arrayDecode


let
  settings =
    [ 
      ([t|Int|], [|PTI.int8|], [|Encoder.int8 . Left . fromIntegral|], [|Decoder.int|]),
      ([t|Int8|], [|PTI.int2|], [|Encoder.int2 . Left . fromIntegral|], [|Decoder.int|]),
      ([t|Int16|], [|PTI.int2|], [|Encoder.int2 . Left|], [|Decoder.int|]),
      ([t|Int32|], [|PTI.int4|], [|Encoder.int4 . Left|], [|Decoder.int|]),
      ([t|Int64|], [|PTI.int8|], [|Encoder.int8 . Left|], [|Decoder.int|]),
      ([t|Word|], [|PTI.int8|], [|Encoder.int8 . Right . fromIntegral|], [|Decoder.int|]),
      ([t|Word8|], [|PTI.int2|], [|Encoder.int2 . Right . fromIntegral|], [|Decoder.int|]),
      ([t|Word16|], [|PTI.int2|], [|Encoder.int2 . Right|], [|Decoder.int|]),
      ([t|Word32|], [|PTI.int4|], [|Encoder.int4 . Right|], [|Decoder.int|]),
      ([t|Word64|], [|PTI.int8|], [|Encoder.int8 . Right|], [|Decoder.int|]),
      ([t|Float|], [|PTI.float4|], [|Encoder.float4|], [|Decoder.float4|]),
      ([t|Double|], [|PTI.float8|], [|Encoder.float8|], [|Decoder.float8|]),
      ([t|Scientific|], [|PTI.numeric|], [|Encoder.numeric|], [|Decoder.numeric|]),
      ([t|Day|], [|PTI.date|], [|Encoder.date|], [|Decoder.date|]),
      ([t|TimeOfDay|], [|PTI.time|], [|Encoder.time|], [|Decoder.time|]),
      ([t|(TimeOfDay, TimeZone)|], [|PTI.timetz|], [|Encoder.timetz|], [|Decoder.timetz|]),
      ([t|UTCTime|], [|PTI.timestamp|], [|Encoder.timestamp|], [|Decoder.timestamp|]),
      ([t|LocalTime|], [|PTI.timestamptz|], [|Encoder.timestamptz|], [|Decoder.timestamptz|]),
      ([t|DiffTime|], [|PTI.interval|], [|Encoder.interval|], [|Decoder.interval|]),
      ([t|Char|], [|PTI.text|], [|Encoder.char|], [|Decoder.char|]),
      ([t|Text|], [|PTI.text|], [|Encoder.text . Left|], [|Decoder.text|]),
      ([t|LazyText|], [|PTI.text|], [|Encoder.text . Right|], [|fmap TL.fromStrict . Decoder.text|]),
      ([t|ByteString|], [|PTI.bytea|], [|Encoder.bytea . Left|], [|Decoder.bytea|]),
      ([t|LazyByteString|], [|PTI.bytea|], [|Encoder.bytea . Right|], [|fmap BL.fromStrict . Decoder.bytea|]),
      ([t|Bool|], [|PTI.bool|], [|Encoder.bool|], [|Decoder.bool|]),
      ([t|UUID|], [|PTI.uuid|], [|Encoder.uuid|], [|Decoder.uuid|])
    ]
  in
    fmap concat $ forM settings $ \(t, pti, encoder, decoder) ->
      [d|

        instance Mapping $t where
          {-# INLINE oid #-}
          oid =
            const $ PTI.ptiOID $pti
          {-# INLINE encode #-}
          encode =
            Just . $encoder
          {-# INLINE decode #-}
          decode x =
            do
              b <- maybe (Left "NULL input") return x
              $decoder b

        instance ArrayMapping $t where
          {-# INLINE arrayOID #-}
          arrayOID =
            const $ fromMaybe ($bug "No array OID") $ PTI.ptiArrayOID $pti
          {-# INLINE arrayEncode #-}
          arrayEncode x =
            Array.fromSingleton (Just ($encoder x))
                                    (False)
                                    (PTI.oidWord32 (PTI.ptiOID $pti))
          {-# INLINE arrayDecode #-}
          arrayDecode =
            \case
              (_, [x], _, _) -> decode x
              x -> Left $ "Array data doesn't match the '" <> 
                          $(t >>= TH.stringE . show) <> "' type: " <> 
                          (fromString . show) x

      |]

