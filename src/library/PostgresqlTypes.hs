-- |
-- Haskell representations of PostgreSQL data structures in their canonical forms that directly correspond to their PostgreSQL definitions. No data loss, no compromise!
--
-- The philosophy behind this package is that nuance matters. PostgreSQL has a rich type system with many types that have subtle differences in their behavior and representation. This package aims to provide Haskell types that accurately reflect these PostgreSQL types, preserving their semantics and constraints.
--
-- These types do not necessarily have direct mappings to the common Haskell types. Canonicalizing conversions and smart constructors are provided to address that.
--
-- E.g., any @text@ value from PostgreSQL makes valid 'Data.Text.Text' values in Haskell, but not every Haskell 'Data.Text.Text` value makes valid PostgreSQL @text@, because PostgreSQL does not allow NUL-bytes in text fields, but Haskell's 'Data.Text.Text' does. In case of dates the supported date ranges may differ between PostgreSQL and Haskell's \"time\" library. Therefore, conversions between these types and common Haskell types may be partial and may fail if the data cannot be represented in the target type.
--
-- = Type Categories
--
-- The types are organized into the following categories:
--
-- == Numeric Types
--
-- * @'Int2'@ - 2-byte signed integer (@int2@ \/ @smallint@)
-- * @'Int4'@ - 4-byte signed integer (@int4@ \/ @integer@)
-- * @'Int8'@ - 8-byte signed integer (@int8@ \/ @bigint@)
-- * @'Float4'@ - Single-precision floating point (@float4@ \/ @real@)
-- * @'Float8'@ - Double-precision floating point (@float8@ \/ @double precision@)
-- * @'Numeric' precision scale@ - Arbitrary and precise precision numeric (@numeric@ \/ @decimal@)
-- * @'Money'@ - Currency amount (@money@)
-- * @'Oid'@ - Object identifier (@oid@)
--
-- == Character Types
--
-- * @'Text'@ - Variable-length character string (@text@)
-- * @'Varchar' limit@ - Variable-length with limit (@varchar@)
-- * @'Char'@ - Single ASCII character (@char@)
-- * @'Bpchar' length@ - Fixed-length character string (@char(n)@, @character(n)@, or @bpchar(n)@)
--
-- == Boolean Type
--
-- * @'Bool'@ - Boolean (@bool@)
--
-- == Binary Data
--
-- * @'Bytea'@ - Binary data (@bytea@)
--
-- == Date\/Time Types
--
-- * @'Date'@ - Calendar date (@date@)
-- * @'Time'@ - Time of day without time zone (@time@)
-- * @'Timestamp'@ - Date and time without time zone (@timestamp@)
-- * @'Timestamptz'@ - Date and time with time zone (@timestamptz@)
-- * @'Timetz'@ - Time of day with time zone (@timetz@)
-- * @'Interval'@ - Time interval (@interval@)
--
-- == Network Address Types
--
-- * @'Inet'@ - IPv4 or IPv6 host address (@inet@)
-- * @'Cidr'@ - IPv4 or IPv6 network address (@cidr@)
-- * @'Macaddr'@ - MAC address (@macaddr@)
-- * @'Macaddr8'@ - MAC address (EUI-64 format) (@macaddr8@)
-- * @'Ip'@ - Generic IP address type
--
-- == Geometric Types
--
-- * @'Point'@ - Point on a plane (@point@)
-- * @'Line'@ - Infinite line (@line@)
-- * @'Lseg'@ - Line segment (@lseg@)
-- * @'Box'@ - Rectangular box (@box@)
-- * @'Path'@ - Geometric path (@path@)
-- * @'Polygon'@ - Closed geometric path (@polygon@)
-- * @'Circle'@ - Circle (@circle@)
--
-- == Bit String Types
--
-- * @'Bit' length@ - Fixed-length bit string (@bit@)
-- * @'Varbit' limit@ - Variable-length bit string (@varbit@)
--
-- == UUID Type
--
-- * @'Uuid'@ - Universally unique identifier (@uuid@)
--
-- == JSON Types
--
-- * @'Json'@ - JSON data (@json@)
-- * @'Jsonb'@ - Binary JSON data (@jsonb@)
--
-- == Key-Value Types
--
-- * @'Hstore'@ - Key-value store (@hstore@)
--
-- == Range Types
--
-- * @'Range'@ - Generic range type (@int4range@, @int8range@, @numrange@, @tsrange@, @tstzrange@, @daterange@)
-- * @'Multirange'@ - Generic multirange type (@int4multirange@, @int8multirange@, etc.)
--
-- = Type Conversions
--
-- These types do not necessarily have direct mappings to common Haskell types,
-- but they provide explicit constructors and accessors for safe conversions.
--
-- The library provides two types of constructors for each type:
--
-- * __Normalizing constructors__ (prefix: @normalizeFrom*@) - Always succeed by clamping or canonicalizing input values to valid ranges
-- * __Refining constructors__ (prefix: @refineFrom*@) - Return 'Maybe', failing if the input is out of range
-- * __Accessor functions__ (prefix: @to*@) - Extract values from PostgreSQL types
--
-- Each type also implements 'IsScalar' which provides binary and textual
-- encoding/decoding according to PostgreSQL's wire protocol specification.
--
-- = Usage Examples
--
-- > import qualified Data.Text as Text
-- > import qualified Data.Time as Time
-- > import qualified PostgresqlTypes.Int4 as Int4
-- > import qualified PostgresqlTypes.Date as Date
-- > import qualified PostgresqlTypes.Text as PgText
-- >
-- > -- Normalizing conversion: Int32 -> Int4 (always succeeds, clamping if needed)
-- > pgInt :: Int4.Int4
-- > pgInt = Int4.normalizeFromInt32 42
-- >
-- > -- Extract Int32 from Int4
-- > hsInt :: Int32
-- > hsInt = Int4.toInt32 pgInt
-- >
-- > -- Refining conversion: Text -> PostgreSQL Text. May fail if contains NUL-bytes.
-- > pgTextMaybe :: Maybe PgText.Text
-- > pgTextMaybe = PgText.refineFromText (Text.pack "Hello")
-- >
-- > -- Normalizing conversion which removes NUL-bytes from input
-- > pgText :: PgText.Text
-- > pgText = PgText.normalizeFromText (Text.pack "Hello")
-- >
-- > -- Normalizing conversion with clamping: Day -> Date (clamps to valid range)
-- > pgDate :: Date.Date
-- > pgDate = Date.normalizeFromDay (Time.fromGregorian 2024 1 15)
-- >
-- > -- Extract Day from Date
-- > hsDay :: Time.Day
-- > hsDay = Date.toDay pgDate
--
-- For more information:
--
-- * [PostgreSQL type documentation](https://www.postgresql.org/docs/current/datatype.html)
module PostgresqlTypes
  ( Bit,
    Bool,
    Box,
    Bpchar,
    Bytea,
    Char,
    Cidr,
    Circle,
    Date,
    Float4,
    Float8,
    Hstore,
    Inet,
    Int2,
    Int4,
    Int8,
    Interval,
    Json,
    Jsonb,
    Line,
    Lseg,
    Macaddr,
    Macaddr8,
    Money,
    Multirange,
    Numeric,
    Oid,
    Path,
    Point,
    Polygon,
    Range,
    Text,
    Time,
    Timestamp,
    Timestamptz,
    Timetz,
    Uuid,
    Varbit,
    Varchar,
  )
where

import PostgresqlTypes.Bit
import PostgresqlTypes.Bool
import PostgresqlTypes.Box
import PostgresqlTypes.Bpchar
import PostgresqlTypes.Bytea
import PostgresqlTypes.Char
import PostgresqlTypes.Cidr
import PostgresqlTypes.Circle
import PostgresqlTypes.Date
import PostgresqlTypes.Float4
import PostgresqlTypes.Float8
import PostgresqlTypes.Hstore
import PostgresqlTypes.Inet
import PostgresqlTypes.Int2
import PostgresqlTypes.Int4
import PostgresqlTypes.Int8
import PostgresqlTypes.Interval
import PostgresqlTypes.Json
import PostgresqlTypes.Jsonb
import PostgresqlTypes.Line
import PostgresqlTypes.Lseg
import PostgresqlTypes.Macaddr
import PostgresqlTypes.Macaddr8
import PostgresqlTypes.Money
import PostgresqlTypes.Multirange
import PostgresqlTypes.Numeric
import PostgresqlTypes.Oid
import PostgresqlTypes.Path
import PostgresqlTypes.Point
import PostgresqlTypes.Polygon
import PostgresqlTypes.Range
import PostgresqlTypes.Text
import PostgresqlTypes.Time
import PostgresqlTypes.Timestamp
import PostgresqlTypes.Timestamptz
import PostgresqlTypes.Timetz
import PostgresqlTypes.Uuid
import PostgresqlTypes.Varbit
import PostgresqlTypes.Varchar
