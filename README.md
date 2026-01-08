# postgresql-types

<!-- [![Hackage](https://img.shields.io/hackage/v/postgresql-types.svg)](https://hackage.haskell.org/package/postgresql-types) -->
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/postgresql-types/)

Haskell representation of PostgreSQL data types with mapping to and from binary format used by the PostgreSQL wire protocol.

## Status

In active development, but already working and exhaustively tested.

## Supported Types

- Numeric types: `Int2`, `Int4`, `Int8`, `Float4`, `Float8`, `Numeric`
- Character types: `Char`, `Varchar`, `Text`
- Boolean type: `Bool`
- Date/time types: `Date`, `Time`, `Timestamp`, `Timestamptz`, `Interval`
- Network address types: `Inet`, `Cidr`, `Macaddr`, `Macaddr8`
- Geometric types: `Point`, `Line`, `Lseg`, `Box`, `Path`, `Polygon`, `Circle`
- Bit string types: `Bit`, `Varbit`
- UUID type: `Uuid`
- JSON types: `Json`, `Jsonb`
- Range types: `Int4Range`, `Int8Range`, `NumRange`, `TsRange`, `TstzRange`, `DateRange`
- Multirange types: `Int4Multirange`, `Int8Multirange`, `NumMultirange`, `TsMultirange`, `TstzMultirange`, `DateMultirange`
- Array types for all of the above
