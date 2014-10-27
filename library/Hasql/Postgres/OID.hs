module Hasql.Postgres.OID where

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
