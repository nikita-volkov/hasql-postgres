-- |
-- Internal mappings.
module Hasql.Postgres.Mapping where

import Hasql.Postgres.Prelude hiding (bool)
import PostgreSQLBinary.Types (PGEnum(..))

import qualified Language.Haskell.TH as TH
import qualified Hasql.Postgres.PTI as PTI
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified PostgreSQLBinary.Array as Array
import qualified PostgreSQLBinary.Encoder as Encoder
import qualified PostgreSQLBinary.Decoder as Decoder


-- |
-- Server settings.
-- 
-- * @integer_datetimes@
type Environment =
  Bool

type Value =
  Maybe ByteString

-- |
-- A final value level mapping.
class Mapping a where
  oid :: a -> PTI.OID
  encode :: Environment -> a -> Value
  decode :: Environment -> Value -> Either Text a

-- |
-- A mapping for construction of array values.
class ArrayMapping a where
  arrayOID :: a -> PTI.OID
  arrayEncode :: Environment -> a -> Array.Data
  arrayDecode :: Environment -> Array.Data -> Either Text a


instance Mapping a => Mapping (Maybe a) where
  oid = 
    const $ oid (undefined :: a)
  encode e = 
    join . traverse (encode e)
  decode e = 
    maybe (return Nothing) (fmap Just . (decode e) . Just)

instance (ArrayMapping a, Mapping a) => ArrayMapping (Maybe a) where
  arrayOID =
    const $ arrayOID (undefined :: a)
  arrayEncode e =
    \case
      Nothing -> 
        Array.fromSingleton Nothing True (PTI.oidWord32 (oid (undefined :: a)))
      Just x ->
        setNullable True $ arrayEncode e x
        where
          setNullable x (dl, vl, _, oid) = (dl, vl, True, oid)
  arrayDecode e =
    \case
      (_, [x], _, _) -> decode e x
      x -> Left $ "Array data doesn't match the 'Maybe' type: " <> (fromString . show) x


instance (Mapping a, ArrayMapping a) => Mapping [a] where
  oid = 
    arrayOID
  encode e = 
    Just . Encoder.array . arrayEncode e
  decode e x = 
    do
      b <- maybe (Left "NULL input") return x
      a <- Decoder.array b
      arrayDecode e a

instance (Mapping a, ArrayMapping a) => ArrayMapping [a] where
  arrayOID =
    const $ arrayOID (undefined :: a)
  arrayEncode e =
    \case
      [] -> ([(0, 1)], [], False, PTI.oidWord32 $ oid (undefined :: a))
      x -> Array.fromListUnsafe . map (arrayEncode e) $ x
  arrayDecode e x =
    traverse (arrayDecode e) $ Array.elements x


instance (Mapping a, ArrayMapping a) => Mapping (Vector a) where
  oid =
    arrayOID
  encode e = 
    Just . Encoder.array . arrayEncode e
  decode e x = 
    do
      b <- maybe (Left "NULL input") return x
      a <- Decoder.array b
      arrayDecode e a

instance (Mapping a, ArrayMapping a) => ArrayMapping (Vector a) where
  arrayOID =
    const $ arrayOID (undefined :: a)
  arrayEncode e =
    arrayEncode e . V.toList
  arrayDecode e =
    fmap V.fromList . arrayDecode e


let
  settings =
    [ 
      (,,,)
        [t|Int|]
        [|PTI.int8|]
        [|const $ Encoder.int8 . Left . fromIntegral|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Int8|]
        [|PTI.int2|]
        [|const $ Encoder.int2 . Left . fromIntegral|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Int16|]
        [|PTI.int2|]
        [|const $ Encoder.int2 . Left|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Int32|]
        [|PTI.int4|]
        [|const $ Encoder.int4 . Left|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Int64|]
        [|PTI.int8|]
        [|const $ Encoder.int8 . Left|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Word|]
        [|PTI.int8|]
        [|const $ Encoder.int8 . Right . fromIntegral|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Word8|]
        [|PTI.int2|]
        [|const $ Encoder.int2 . Right . fromIntegral|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Word16|]
        [|PTI.int2|]
        [|const $ Encoder.int2 . Right|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Word32|]
        [|PTI.int4|]
        [|const $ Encoder.int4 . Right|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Word64|]
        [|PTI.int8|]
        [|const $ Encoder.int8 . Right|]
        [|const $ Decoder.int|]
      ,
      (,,,)
        [t|Float|]
        [|PTI.float4|]
        [|const $ Encoder.float4|]
        [|const $ Decoder.float4|]
      ,
      (,,,)
        [t|Double|]
        [|PTI.float8|]
        [|const $ Encoder.float8|]
        [|const $ Decoder.float8|]
      ,
      (,,,)
        [t|Scientific|]
        [|PTI.numeric|]
        [|const $ Encoder.numeric|]
        [|const $ Decoder.numeric|]
      ,
      (,,,)
        [t|Day|]
        [|PTI.date|]
        [|const $ Encoder.date|]
        [|const $ Decoder.date|]
      ,
      (,,,)
        [t|TimeOfDay|]
        [|PTI.time|]
        [|Encoder.time|]
        [|Decoder.time|]
      ,
      (,,,)
        [t|(TimeOfDay, TimeZone)|]
        [|PTI.timetz|]
        [|Encoder.timetz|]
        [|Decoder.timetz|]
      ,
      (,,,)
        [t|LocalTime|]
        [|PTI.timestamp|]
        [|Encoder.timestamp|]
        [|Decoder.timestamp|]
      ,
      (,,,)
        [t|UTCTime|]
        [|PTI.timestamptz|]
        [|Encoder.timestamptz|]
        [|Decoder.timestamptz|]
      ,
      (,,,)
        [t|DiffTime|]
        [|PTI.interval|]
        [|Encoder.interval|]
        [|Decoder.interval|]
      ,
      (,,,)
        [t|Char|]
        [|PTI.text|]
        [|const $ Encoder.char|]
        [|const $ Decoder.char|]
      ,
      (,,,)
        [t|Text|]
        [|PTI.text|]
        [|const $ Encoder.text . Left|]
        [|const $ Decoder.text|]
      ,
      (,,,)
        [t|LazyText|]
        [|PTI.text|]
        [|const $ Encoder.text . Right|]
        [|const $ fmap TL.fromStrict . Decoder.text|]
      ,
      (,,,)
        [t|PGEnum|]
        [|PTI.enum|]
        [|const $ Encoder.text . Left . getEnumText|]
        [|const $ fmap PGEnum . Decoder.text|]
      ,
      (,,,)
        [t|ByteString|]
        [|PTI.bytea|]
        [|const $ Encoder.bytea . Left|]
        [|const $ Decoder.bytea|]
      ,
      (,,,)
        [t|LazyByteString|]
        [|PTI.bytea|]
        [|const $ Encoder.bytea . Right|]
        [|const $ fmap BL.fromStrict . Decoder.bytea|]
      ,
      (,,,)
        [t|Bool|]
        [|PTI.bool|]
        [|const $ Encoder.bool|]
        [|const $ Decoder.bool|]
      ,
      (,,,)
        [t|UUID|]
        [|PTI.uuid|]
        [|const $ Encoder.uuid|]
        [|const $ Decoder.uuid|]
    ]
  in
    fmap concat $ forM settings $ \(t, pti, encoder, decoder) ->
      [d|

        instance Mapping $t where
          {-# INLINE oid #-}
          oid =
            const $ PTI.ptiOID $pti
          {-# INLINE encode #-}
          encode e =
            Just . $encoder e
          {-# INLINE decode #-}
          decode e x =
            do
              b <- maybe (Left "NULL input") return x
              $decoder e b

        instance ArrayMapping $t where
          {-# INLINE arrayOID #-}
          arrayOID =
            const $ fromMaybe ($bug "No array OID") $ PTI.ptiArrayOID $pti
          {-# INLINE arrayEncode #-}
          arrayEncode e x =
            Array.fromSingleton (Just ($encoder e x))
                                (False)
                                (PTI.oidWord32 (PTI.ptiOID $pti))
          {-# INLINE arrayDecode #-}
          arrayDecode e =
            \case
              (_, [x], _, _) -> decode e x
              x -> Left $ "Array data doesn't match the '" <> 
                          $(t >>= TH.stringE . show) <> "' type: " <> 
                          (fromString . show) x

      |]

