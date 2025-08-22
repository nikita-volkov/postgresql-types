# GitHub Copilot Instructions for postgresql-types

## Project Overview

This is a Haskell library that provides type mappings and integrations for PostgreSQL types. The library supports both binary and text format codecs as per PostgreSQL terminology and serves as a foundation for various PostgreSQL libraries (e.g., used in the "hasql" library).

## Project Structure

This is a multi-package Cabal project with the following components:

### Core Libraries
- **primitive-layer**: Core PostgreSQL type mappings with binary/text encoders and decoders
- **declaration-layer**: Higher-level abstractions and declarative APIs 
- **jsonifier-aeson**: Integration between Jsonifier and Aeson for JSON handling
- **testcontainers-postgresql**: PostgreSQL test container utilities

### Test Suites
- **unit-tests**: Unit tests for type conversions and properties
- **integration-tests**: Integration tests against real PostgreSQL instances

### File Organization
```
src/
├── primitive-layer/           # Core type definitions and codecs
│   ├── PrimitiveLayer/
│   │   ├── Algebra.hs        # Core algebraic structures
│   │   ├── Types/       # Individual type implementations
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

### Type Classes and Patterns
- `Mapping` typeclass defines PostgreSQL type mappings with OIDs, encoders, and decoders
- Heavy use of `Tagged` types for type-safe OID associations
- Binary encoders use `PtrPoker.Write` for efficient memory operations
- Binary decoders use `PeekyBlinders` for safe parsing
- Text encoders use `TextBuilder` for efficient string construction

### Code Style
- Uses extensive language extensions (see `postgresql-types.cabal`)
- Custom Prelude modules for consistent imports
- NoImplicitPrelude with explicit re-exports
- Strict data fields and unboxed tuples for performance. No bangs in data-types
- QuickCheck properties for testing roundtrip conversions

## Key Dependencies and Documentation

### References
- [PostgreSQL types docs](https://www.postgresql.org/docs/17/datatype.html) - contains a list of standard types and documentation on them
- [libpqtypes library source code](https://github.com/pgagarinov/libpqtypes) - contains codecs for various types implemented in C
- [PostgreSQL source](https://github.com/postgres/postgres)
   - Pay attention to [backend](https://github.com/postgres/postgres/tree/master/src/backend). It contains the code dealing with encoding of types and their structure.
- ["peeky-blinders" source code](https://github.com/nikita-volkov/peeky-blinders)
- ["ptr-poker" Hackage docs](https://hackage.haskell.org/package/ptr-poker)

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
   - IsSome must be injective
   - Is must be isomorphic

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