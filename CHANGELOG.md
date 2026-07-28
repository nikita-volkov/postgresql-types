# v0.1.7.0

## Non-breaking

- Added `Foldable` and `Traversable` instances for `PostgresqlTypes.Range` and `PostgresqlTypes.Multirange`.

# v0.1.6.0

## Non-breaking

- Added `refineFromLocalTime` to `PostgresqlTypes.Timestamp` and `refineFromUtcTime` to `PostgresqlTypes.Timestamptz`, mirroring the `refineFromX` pattern already present for `Text`, `Date`, `Time`, and `Numeric`. Each validates that the input is both within the representable range and round-trippable without loss of sub-microsecond precision, returning `Nothing` otherwise.

# v0.1.5.1

## Fixes

- Fixed `Geometry`'s `Arbitrary` instance nesting unscaled lists inside each other (e.g. `MultiPolygonShape`, `CurvePolygonShape`), which multiplied the QuickCheck size parameter against itself and made the PostGIS integration tests extremely slow.

# v0.1.5.0

## Non-breaking

- Upgraded to `postgresql-types-algebra` v0.2. `IsScalar` is renamed to `IsPrimitive` and no longer carries `binaryEncoder`/`binaryDecoder`; those move to the new `IsBinaryPrimitive` subclass. A type with no PostgreSQL binary wire format now simply lacks an `IsBinaryPrimitive` instance instead of the class being total. `PostgresqlTypes.Via.IsScalar` is renamed to `PostgresqlTypes.Via.IsPrimitive`, and `ViaIsScalar` to `ViaIsPrimitive`. All built-in types (including `Range`, `Multirange`, and `Varchar`) implement both classes, so this is transparent to callers who don't reference `IsScalar`/`ViaIsScalar` by name.

# v0.1.4.0

## Non-breaking

- Added `PostgresqlTypes.Geometry` module for the PostGIS extension type covering all 16 PostGIS/OGC geometry kinds: added `CircularString`, `Triangle`, `PolyhedralSurface`, `TIN`, `CompoundCurve`, `CurvePolygon`, `MultiCurve`, and `MultiSurface` support (#72, #73, #74, #75, #76, #77, #78)
  - Added `NurbsCurveShape` support for ISO/IEC 13249-3:2016 NURBS curves; untested against any real PostGIS server since none has shipped the format yet, and it may still change upstream (#79)
