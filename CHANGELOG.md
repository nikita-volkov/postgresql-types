# v0.1.4.0

## Non-breaking

- Added `PostgresqlTypes.Geometry` module for the PostGIS extension type covering all 16 PostGIS/OGC geometry kinds: added `CircularString`, `Triangle`, `PolyhedralSurface`, `TIN`, `CompoundCurve`, `CurvePolygon`, `MultiCurve`, and `MultiSurface` support (#72, #73, #74, #75, #76, #77, #78)
  - Added `NurbsCurveShape` support for ISO/IEC 13249-3:2016 NURBS curves; untested against any real PostGIS server since none has shipped the format yet, and it may still change upstream (#79)
