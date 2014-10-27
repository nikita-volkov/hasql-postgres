module Hasql.Postgres.OID where

import Hasql.Postgres.Prelude hiding (bool)
import Database.PostgreSQL.LibPQ (Oid(..))


type OID = Oid

abstime     :: OID = Oid 702
anyarray    :: OID = Oid 2277
bit         :: OID = Oid 1560
bool        :: OID = Oid 16
box         :: OID = Oid 603
bpchar      :: OID = Oid 1042
bytea       :: OID = Oid 17
char        :: OID = Oid 18
cid         :: OID = Oid 29
cidr        :: OID = Oid 650
circle      :: OID = Oid 718
date        :: OID = Oid 1082
float4      :: OID = Oid 700
float8      :: OID = Oid 701
gtsvector   :: OID = Oid 3642
inet        :: OID = Oid 869
int2        :: OID = Oid 21
int2vector  :: OID = Oid 22
int4        :: OID = Oid 23
int8        :: OID = Oid 20
interval    :: OID = Oid 1186
json        :: OID = Oid 114
line        :: OID = Oid 628
lseg        :: OID = Oid 601
macaddr     :: OID = Oid 829
money       :: OID = Oid 790
name        :: OID = Oid 19
numeric     :: OID = Oid 1700
oid         :: OID = Oid 26
path        :: OID = Oid 602
point       :: OID = Oid 600
polygon     :: OID = Oid 604
record      :: OID = Oid 2249
refcursor   :: OID = Oid 1790
regproc     :: OID = Oid 24
reltime     :: OID = Oid 703
text        :: OID = Oid 25
tid         :: OID = Oid 27
time        :: OID = Oid 1083
timestamp   :: OID = Oid 1114
timestamptz :: OID = Oid 1184
timetz      :: OID = Oid 1266
tinterval   :: OID = Oid 704
tsvector    :: OID = Oid 3614
unknown     :: OID = Oid 705
uuid        :: OID = Oid 2950
varbit      :: OID = Oid 1562
varchar     :: OID = Oid 1043
void        :: OID = Oid 2278
xid         :: OID = Oid 28
xml         :: OID = Oid 142

toArrayOID :: OID -> Maybe OID
toArrayOID (Oid i) =
  fmap Oid $ case i of
    16   -> Just 1000
    17   -> Just 1001
    18   -> Just 1002
    19   -> Just 1003
    20   -> Just 1016
    21   -> Just 1005
    22   -> Just 1006
    23   -> Just 1007
    24   -> Just 1008
    25   -> Just 1009
    26   -> Just 1028
    27   -> Just 1010
    28   -> Just 1011
    29   -> Just 1012
    30   -> Just 1013
    114  -> Just 199
    142  -> Just 143
    600  -> Just 1017
    601  -> Just 1018
    602  -> Just 1019
    603  -> Just 1020
    604  -> Just 1027
    628  -> Just 629
    700  -> Just 1021
    701  -> Just 1022
    702  -> Just 1023
    703  -> Just 1024
    704  -> Just 1025
    718  -> Just 719
    790  -> Just 791
    829  -> Just 1040
    869  -> Just 1041
    650  -> Just 651
    1033 -> Just 1034
    1042 -> Just 1014
    1043 -> Just 1015
    1082 -> Just 1182
    1083 -> Just 1183
    1114 -> Just 1115
    1184 -> Just 1185
    1186 -> Just 1187
    1266 -> Just 1270
    1560 -> Just 1561
    1562 -> Just 1563
    1700 -> Just 1231
    1790 -> Just 2201
    2202 -> Just 2207
    2203 -> Just 2208
    2204 -> Just 2209
    2205 -> Just 2210
    2206 -> Just 2211
    2950 -> Just 2951
    3614 -> Just 3643
    3642 -> Just 3644
    3615 -> Just 3645
    3734 -> Just 3735
    3769 -> Just 3770
    2970 -> Just 2949
    3904 -> Just 3905
    3906 -> Just 3907
    3908 -> Just 3909
    3910 -> Just 3911
    3912 -> Just 3913
    3926 -> Just 3927
    2249 -> Just 2287
    2275 -> Just 1263
    _    -> Nothing


class Identifiable a where
  identifyOID :: a -> OID
  identifyArrayOID :: a -> OID
  identifyArrayOID = fromMaybe unknown . toArrayOID . identifyOID

instance Identifiable a => Identifiable (Maybe a) where
  identifyOID = const $ identifyOID (undefined :: a)
  identifyArrayOID = const $ identifyArrayOID (undefined :: a)

instance Identifiable a => Identifiable (Vector a) where
  identifyOID = const $ identifyArrayOID (undefined :: a)
  identifyArrayOID = identifyOID

instance Identifiable Bool where
  identifyOID = const bool

instance Identifiable Int where
  identifyOID = const int8

instance Identifiable Int8 where
  identifyOID = const int2

instance Identifiable Int16 where
  identifyOID = const int2

instance Identifiable Int32 where
  identifyOID = const int4

instance Identifiable Int64 where
  identifyOID = const int8

instance Identifiable Word where
  identifyOID = const int8

instance Identifiable Word8 where
  identifyOID = const int2

instance Identifiable Word16 where
  identifyOID = const int4

instance Identifiable Word32 where
  identifyOID = const int8

instance Identifiable Word64 where
  identifyOID = const int8

instance Identifiable Float where
  identifyOID = const float4

instance Identifiable Double where
  identifyOID = const float8

instance Identifiable Scientific where
  identifyOID = const numeric

instance Identifiable Day where
  identifyOID = const date

instance Identifiable TimeOfDay where
  identifyOID = const time

instance Identifiable LocalTime where
  identifyOID = const timestamp

instance Identifiable ZonedTime where
  identifyOID = const timestamptz

instance Identifiable UTCTime where
  identifyOID = const timestamp

instance Identifiable Char where
  identifyOID = const varchar

instance Identifiable Text where
  identifyOID = const text

instance Identifiable LazyText where
  identifyOID = const text

instance Identifiable ByteString where
  identifyOID = const bytea

instance Identifiable LazyByteString where
  identifyOID = const bytea
