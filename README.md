# postgresql-types

<!-- [![Hackage](https://img.shields.io/hackage/v/postgresql-types.svg)](https://hackage.haskell.org/package/postgresql-types) -->
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/postgresql-types/)

Haskell representation of PostgreSQL data types with mappings to and from both binary and textual formats used by the PostgreSQL wire protocol.

## Status

In active development, but already working and exhaustively tested.

## Key Features

### Supported Types

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

### Type Safety & Valid Ranges

All PostgreSQL types are represented with hidden constructors, ensuring that only valid PostgreSQL values can be constructed. This design prevents invalid data from being represented at the type level.

Values can only be created through lawful conversions, guaranteeing adherence to PostgreSQL's constraints (e.g., dates within PostgreSQL's supported range, text without NUL bytes).

### Lawful Conversions

The library uses the [lawful-conversions](https://hackage.haskell.org/package/lawful-conversions) library to provide safe, well-defined transformations between Haskell and PostgreSQL types:

- **`IsMany`** - Total conversions that always succeed (e.g., `Int32 → Int4`)
- **`IsSome`** - Partial conversions that may fail (e.g., `Text → Maybe PostgreSQL.Text` fails if input contains NUL bytes)
- **`Is`** - Bidirectional isomorphisms combining both directions

This approach ensures that:
- Type constructors remain hidden to protect invariants
- All conversions are explicit and lawful
- Invalid data is either rejected or canonicalized (e.g., removing NUL bytes from text)
- Round-trip properties are maintained

### Complete Range Coverage with Property Testing

Every type implements `Arbitrary` instances that **simulate the complete range of valid PostgreSQL values**, not just convenient Haskell subsets. For example:

- **`Date`** generates values spanning PostgreSQL's full range: 4713 BC to 5874897 AD
- **`Text`** generates strings excluding NUL characters (as PostgreSQL doesn't allow them)
- **`Circle`** generates circles with non-negative radii
- **`Numeric`** covers arbitrary-precision numbers including edge cases

This comprehensive approach to test data generation ensures that the library is tested against the actual constraints and edge cases of PostgreSQL, not just typical use cases.

### Exhaustive Testing

The library employs a rigorous multi-layered testing strategy:

#### Unit Tests
- **Encoder/Decoder Round-trips**: Every type is tested for round-trip fidelity through both binary and textual encodings
- **Lawful Conversion Properties**: All conversion instances are validated against their lawful properties using property-based testing
- **Cross-format Validation**: Binary encoders are tested against textual decoders and vice versa

#### Integration Tests
- **Real PostgreSQL Validation**: Tests run against actual PostgreSQL servers (versions 9 through 17)
- **Four-way Round-trip Matrix**: Each value is tested through all four combinations:
  1. Text encoding → Text decoding
  2. Text encoding → Binary decoding  
  3. Binary encoding → Text decoding
  4. Binary encoding → Binary decoding
- **Server Canonical Form**: All encodings are validated by sending values to PostgreSQL and verifying the server produces the expected output
- **Array Support**: Every type is tested both as a scalar and as array elements
- **Metadata Validation**: Type OIDs are verified against the PostgreSQL system catalog

This comprehensive testing model provides confidence that the library correctly handles all PostgreSQL types across all encoding formats and PostgreSQL versions.
