# GitHub Copilot Instructions for postgresql-types

## Project Overview

This is a high-performance Haskell library that provides comprehensive type mappings and integrations for PostgreSQL types. The library implements both binary and text format codecs according to PostgreSQL's wire protocol specification and serves as a foundational component for various PostgreSQL client libraries, most notably the "hasql" ecosystem.

## Project Structure

This is a multi-package Cabal project with the following components:

### Core Libraries
- **primitive-layer**: Core PostgreSQL type mappings with efficient binary/text encoders and decoders
- **declaration-layer**: Higher-level abstractions and declarative APIs built on the primitive layer
- **jsonifier-aeson**: JSON integration bridging Jsonifier and Aeson libraries for efficient JSON handling
- **testcontainers-postgresql**: PostgreSQL test container utilities for integration testing

### Test Suites
- **unit-tests**: Unit tests for type conversions and properties
- **integration-tests**: Integration tests against real PostgreSQL instances

### File Organization
```
src/
├── primitive-layer/           # Core type definitions and codecs
│   ├── PrimitiveLayer/
│   │   ├── Algebra.hs        # Core algebraic structures
│   │   ├── Types/            # Individual type implementations
│   │   └── Prelude.hs        # Common imports and utilities
├── declaration-layer/         # High-level declarative APIs
├── jsonifier-aeson/          # JSON integration
└── testcontainers-postgresql/ # Test infrastructure
```

## Architecture

### Layered Design
The library follows a layered architecture:

1. **Primitive Layer**: Low-level type mappings with binary/text codecs
2. **Declaration Layer**: High-level declarative APIs built on primitives
3. **Integration Layers**: Specific integrations (JSON, testing, etc.)

### Core Concepts
- **Wire Protocol Compliance**: All codecs follow PostgreSQL's wire protocol specification
- **Type Safety**: Extensive use of phantom types and lawful conversions
- **Performance**: Zero-copy parsing and efficient encoding where possible
- **Correctness**: Property-based testing ensures codec reliability

### Type Classes and Patterns
- **`Mapping` typeclass**: Defines PostgreSQL type mappings with OIDs, encoders, and decoders
- **Tagged types**: Extensive use of `Tagged` types for type-safe OID associations
- **Binary encoding**: Uses `PtrPoker.Write` for efficient memory operations
- **Binary decoding**: Uses `PeekyBlinders` for safe parsing with proper error handling
- **Text encoding**: Uses `TextBuilder` for efficient string construction

### Code Style
- Uses extensive language extensions (see `postgresql-types.cabal`)
- Custom Prelude modules for consistent imports
- `NoImplicitPrelude` with explicit re-exports
- Strict data fields and unboxed tuples for performance (no bangs in data types)
- QuickCheck properties for testing roundtrip conversions

### Lawful Conversions
The library emphasizes type safety through lawful conversions:

- **Hide constructors**: Mapping type constructors are hidden in favor of lawful conversions
  - Makes types safer by preventing construction of invalid values
  - Provides API stability while allowing flexible underlying representations
- **`IsSome` instances**: For smart construction and extraction of types
- **`IsMany` instances**: Where normalizing construction is possible
- **`Is` instances**: Where the underlying representation is isomorphic

### Arbitrary Instances
- All mapping types must have an `Arbitrary` instance that strictly follows constraints
- Instances must not reuse lawful-conversions since they are intended to test them
- Instances MUST NOT avoid any values to make the tests pass

## Key Dependencies and Documentation

### References
- [PostgreSQL types documentation](https://www.postgresql.org/docs/17/datatype.html) - Complete list of standard types and documentation
- [libpqtypes library source](https://github.com/pgagarinov/libpqtypes) - Reference C implementations of various type codecs
- [PostgreSQL source code](https://github.com/postgres/postgres)
  - Focus on [backend](https://github.com/postgres/postgres/tree/master/src/backend) - Contains encoding logic and type structure implementations
- [peeky-blinders source](https://github.com/nikita-volkov/peeky-blinders) - Binary parsing library
- [ptr-poker documentation](https://hackage.haskell.org/package/ptr-poker) - Binary encoding library

### Core Dependencies
- **[text-builder](https://hackage.haskell.org/package/text-builder)**: Efficient text construction for encoders
- **[peeky-blinders](https://hackage.haskell.org/package/peeky-blinders)**: Safe binary parsing for decoders  
- **[ptr-poker](https://hackage.haskell.org/package/ptr-poker)**: Efficient binary encoding
- **[aeson](https://hackage.haskell.org/package/aeson)**: JSON parsing and generation
- **[lawful-conversions](https://hackage.haskell.org/package/lawful-conversions)**: Type-safe conversions between related types
- **[vector](https://hackage.haskell.org/package/vector)**: Efficient arrays
- **[scientific](https://hackage.haskell.org/package/scientific)**: Arbitrary precision numbers
- **[uuid](https://hackage.haskell.org/package/uuid)**: UUID type support

### Testing Dependencies  
- **[QuickCheck](https://hackage.haskell.org/package/QuickCheck)**: Property-based testing
- **[hspec](https://hackage.haskell.org/package/hspec)**: Testing framework
- **[testcontainers](https://hackage.haskell.org/package/testcontainers)**: Container-based testing

## Common Development Tasks

### Analysis
- Avoid running repl (`ghci` or `cabal repl`) to extract info. Use other means.

### Building
```bash
cabal build                    # Build all components
cabal build primitive-layer    # Build specific component
```

### Testing
```bash
cabal test unit-tests         # Run unit tests  
cabal test integration-tests  # Run integration tests (requires Docker)
cabal test --test-show-details=direct  # Show detailed test output
```

### Adding New PostgreSQL Types
1. Create new module in `src/primitive-layer/PrimitiveLayer/Types/`
2. Implement `Mapping` instance with:
   - `typeName`: PostgreSQL type name
   - `baseOid`: PostgreSQL type OID  
   - `arrayOid`: Array type OID
   - `binaryEncoder`/`binaryDecoder`: Binary format codecs
   - `textualEncoder`: Text format encoder
3. Add module to `PrimitiveLayer.Types` exports
4. Add integration test in `src/integration-tests/Main.hs`

### Codec Implementation Patterns
- Use `PtrPoker.Write` combinators for binary encoding
- Use `PeekyBlinders` parsers for binary decoding
   - Group the `statically` blocks in PeekyBlinders. E.g.,

      Instead of

      ```haskell
      do
         micros <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
         days <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
         months <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
      ```

      do

      ```haskell
      PeekyBlinders.statically do
         micros <- PeekyBlinders.beSignedInt8
         days <- PeekyBlinders.beSignedInt4
         months <- PeekyBlinders.beSignedInt4
      ```

- Use `TextBuilder` for text encoding
- Handle null bytes appropriately (PostgreSQL limitation)
- Include proper error handling with `DecodingError`
- Make sure to implement the "lawful-conversions" instances lawfully
   - `IsSome` must be injective
   - `Is` must be isomorphic

## Testing Patterns

### Property-Based Testing
- All primitive types should have `Arbitrary` instances
- Test roundtrip properties: `decode . encode = id`
- Test both binary and text formats
- Use `lawful-conversions` for testing type conversions

### Integration Testing
- Tests run against real PostgreSQL in Docker containers
- Use `TestcontainersPostgresql.with` helper
- Test with various PostgreSQL versions and configurations

## Build Configuration

### GHC Extensions
The project uses many modern Haskell extensions. Key ones include:
- `NoImplicitPrelude`: Custom prelude modules
- `OverloadedStrings`: String literals for various types
- `TypeApplications`: Type application syntax
- `DerivingVia`: Custom deriving strategies
- `RecordWildCards`: Record pattern matching

### Cabal Configuration
- Multi-package project with internal libraries
- Version bounds allow some flexibility while ensuring compatibility
- Uses source-repository-package for `peeky-blinders` dependency

## Contribution Guidelines

### Code Style
- Follow existing patterns in similar modules
- Use the project's custom Prelude modules
- Include comprehensive QuickCheck properties
- Document any PostgreSQL-specific behavior or limitations
- Handle edge cases like null bytes in text data

### Testing Requirements
- All new primitives need integration tests
- Include both positive and negative test cases
- Test boundary conditions and edge cases
- Ensure properties hold for arbitrary generated data

### Performance Considerations
- Use strict data fields for primitive types
- Leverage efficient libraries (TextBuilder, PtrPoker, etc.)
- Avoid unnecessary allocations in hot paths
- Consider memory layout for binary codecs

## Debugging and Development Tips

### Common Issues
- **Null byte handling**: PostgreSQL text format doesn't support null bytes (`\0`) - handle appropriately
- **OID mismatches**: Ensure correct OID assignments for custom types
- **Endianness**: Binary codecs must handle PostgreSQL's network byte order (big-endian)
- **Array encoding**: Array types have different OIDs and require special handling

### Development Workflow
1. **Start with tests**: Write property tests before implementing codecs
2. **Binary first**: Implement binary codecs before text (more efficient, better documented)
3. **Reference PostgreSQL source**: Check backend implementation for edge cases
4. **Integration testing**: Always test against real PostgreSQL instances