# postgresql-types

[![Hackage](https://img.shields.io/hackage/v/postgresql-types.svg)](https://hackage.haskell.org/package/postgresql-types)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/postgresql-types/)

Precise Haskell representations of PostgreSQL data types with mappings to and from both binary and textual formats.

## Status

In active development, but already working and exhaustively tested.

## Key Features

### Ecosystem Integration

Adapter packages provide integrations for major PostgreSQL client libraries:

- "hasql": ["hasql-postgresql-types"](https://github.com/nikita-volkov/hasql-postgresql-types)
- "postgresql-simple": ["postgresql-simple-postgresql-types"](https://github.com/nikita-volkov/postgresql-simple-postgresql-types)

### Supported Types

This package provides support for nearly all PostgreSQL data types, including but not limited to:

- Numeric types: `Int2`, `Int4`, `Int8`, `Float4`, `Float8`, `Numeric`
- Character types: `Char`, `Varchar`, `Text`
- Boolean type: `Bool`
- Date/time types: `Date`, `Time`, `Timestamp`, `Timestamptz`, `Interval`
- Network address types: `Inet`, `Cidr`, `Macaddr`, `Macaddr8`
- Geometric types: `Point`, `Line`, `Lseg`, `Box`, `Path`, `Polygon`, `Circle`
- Bit string types: `Bit`, `Varbit`
- UUID type: `Uuid`
- JSON types: `Json`, `Jsonb`
- Key-value types: `Hstore`
- Range types: `Int4Range`, `Int8Range`, `NumRange`, `TsRange`, `TstzRange`, `DateRange`
- Multirange types: `Int4Multirange`, `Int8Multirange`, `NumMultirange`, `TsMultirange`, `TstzMultirange`, `DateMultirange`
- Array types for all of the above

### Type Safety & Valid Ranges

All PostgreSQL types are represented with hidden constructors, ensuring that only valid PostgreSQL values can be constructed. This design prevents invalid data from being represented at the type level.

Values can only be created through explicit constructor functions, guaranteeing adherence to PostgreSQL's constraints (e.g., dates within PostgreSQL's supported range, text without NUL bytes).

### Type-Safe Constructors

The library provides two types of constructors for each type:

- **Normalizing constructors** (prefix: `normalizeFrom*`) - Always succeed by clamping or canonicalizing input values to valid ranges
- **Refining constructors** (prefix: `refineFrom*`) - Return `Maybe`, failing if the input is out of range

This approach ensures that:
- Type constructors remain hidden to protect invariants
- All conversions are explicit and type-safe
- Invalid data is either rejected or canonicalized (e.g., removing NUL bytes from text, clamping dates to valid range)
- Round-trip properties are maintained via accessor functions

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
- **Constructor Properties**: All constructor functions are validated for proper normalization and refinement behavior using property-based testing
- **Cross-format Validation**: Binary encoders are tested against textual decoders and vice versa

#### Integration Tests
- **Real PostgreSQL Validation**: Tests run against actual PostgreSQL servers (versions 9 through 18)
- **Four-way Round-trip Matrix**: Each value is tested through all four combinations:
  1. Text encoding → Text decoding
  2. Text encoding → Binary decoding  
  3. Binary encoding → Text decoding
  4. Binary encoding → Binary decoding
- **Server Canonical Form**: All encodings are validated by sending values to PostgreSQL and verifying the server produces the expected output
- **Array Support**: Every type is tested both as a scalar and as array elements
- **Metadata Validation**: Type OIDs are verified against the PostgreSQL system catalog

This comprehensive testing model provides confidence that the library correctly handles all PostgreSQL types across all encoding formats and PostgreSQL versions.
