module PostgresqlTypes.Geometry
  ( Geometry,
    Shape (..),
    Coord (..),
    NurbsCurve (..),
    CurveSegment (..),
    Curve (..),
    Surface (..),

    -- * Accessors
    toSrid,
    toShape,

    -- * Constructors
    refineFromShape,
    refineFromShapeAndSrid,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostGIS @geometry@ extension type. A 'Shape' with an optional spatial reference identifier (SRID).
--
-- All coordinates of one geometry share a single dimensionality: every 'Coord' in the shape tree uses the same constructor.
-- This is what PostGIS itself guarantees and what its wire format can express, since the dimensionality is carried once, in the type header.
-- The 'refineFromShape' and 'refineFromShapeAndSrid' constructors enforce it.
--
-- The binary format is [EWKB](https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT) — the format @geometry_send@ emits and @geometry_recv@ accepts.
-- Output is little-endian (NDR); input is accepted in either byte order.
-- The textual format is the hexadecimal encoding of the same payload, which is what @geometry_out@ produces and @geometry_in@ accepts.
--
-- Unlike the built-in PostgreSQL types, @geometry@ is registered by @CREATE EXTENSION postgis@ and receives a different OID in every database.
-- 'baseOid' and 'arrayOid' are therefore 'Nothing', and drivers are expected to resolve the OID by 'typeName' at runtime.
--
-- __Full shape coverage.__ 'Shape' supports every geometry kind PostGIS's @geometry@ column can hold — all
-- sixteen @LWTYPE@ kinds, from the seven basic OGC shapes (@Point@, @LineString@, @Polygon@, @MultiPoint@,
-- @MultiLineString@, @MultiPolygon@, @GeometryCollection@) through the ISO\/SQL-MM curve and surface extensions
-- (@CircularString@, @CompoundCurve@, @CurvePolygon@, @MultiCurve@, @MultiSurface@, @PolyhedralSurface@,
-- @Triangle@, @TIN@) and NURBS curves. See 'Shape'\'s own Haddock for the full constructor list.
--
-- __NURBS curves__ ('NurbsCurveShape') are the one shape not validated against a real PostGIS server: support for
-- them only landed in PostGIS's development branch in June 2026, no released PostGIS version speaks the wire
-- format yet, and that format has seen several revisions and may still change upstream. This codec is derived
-- from the ISO/IEC 13249-3:2016 spec and hand-built fixtures alone. Accordingly, 'NurbsCurveShape' is deliberately
-- excluded from the 'Arbitrary'\/'shrink' instances below and from the real-server integration test suite.
--
-- [PostGIS docs](https://postgis.net/docs/geometry.html).
data Geometry
  = Geometry
      -- | Spatial reference identifier.
      (Maybe Int32)
      -- | Shape.
      Shape
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsPrimitive Geometry)

-- | One of the sixteen OGC\/ISO geometry kinds that a 'Geometry' can hold — PostGIS's complete @LWTYPE@
-- vocabulary, from the seven basic OGC shapes through the ISO\/SQL-MM curve, surface and TIN extensions.
-- See the note on 'Geometry' for the full-coverage summary and the 'NurbsCurveShape' caveat.
data Shape
  = -- | Single coordinate.
    PointShape Coord
  | -- | Line connecting two or more coordinates.
    LineStringShape [Coord]
  | -- | One or more linear rings. The first is the exterior ring, the rest are interior rings (holes).
    -- Every ring is closed: its last coordinate repeats its first.
    PolygonShape [[Coord]]
  | -- | Circular arc string: a sequence of circular arcs, each defined by three consecutive
    -- coordinates (start, midpoint on the arc, end), where the end of one arc is the start of the
    -- next. Requires an odd number of coordinates, at least three.
    CircularStringShape [Coord]
  | -- | Collection of points.
    MultiPointShape [Coord]
  | -- | Collection of line strings.
    MultiLineStringShape [[Coord]]
  | -- | Collection of polygons, each given as its list of rings.
    MultiPolygonShape [[[Coord]]]
  | -- | Collection of polygon faces forming a surface, each given as its list of rings.
    PolyhedralSurfaceShape [[[Coord]]]
  | -- | Heterogeneous collection of shapes.
    --
    -- Members do not carry their own SRID: they inherit the one of the enclosing 'Geometry'.
    GeometryCollectionShape [Shape]
  | -- | A single linear ring, or none. Unlike 'PolygonShape', a triangle admits at most one ring: no
    -- holes, and no more than one exterior boundary. The empty list represents a triangle with no ring
    -- at all; a non-empty one is closed, its last coordinate repeating its first.
    TriangleShape [Coord]
  | -- | Non-Uniform Rational B-Spline curve (ISO/IEC 13249-3:2016).
    --
    -- __Caution:__ untested against any real PostGIS server. See the note on 'Geometry'.
    NurbsCurveShape NurbsCurve
  | -- | Triangulated irregular network: a homogeneous collection of triangles, each given as its
    -- ring, matching 'TriangleShape'\'s own representation — the empty list is a triangle with no
    -- ring at all, a non-empty one is closed with its last coordinate repeating its first.
    TinShape [[Coord]]
  | -- | Curve assembled by chaining line and arc segments end to end.
    --
    -- Members are restricted to 'CurveSegment' rather than 'Shape': unlike
    -- 'GeometryCollectionShape', which legitimately allows arbitrary nesting per OGC, a compound
    -- curve's members can only be a @LineString@ or a @CircularString@, never another
    -- @CompoundCurve@ or anything else. 'CurveSegment' makes that illegal nesting unrepresentable
    -- at compile time, the same reasoning that already led 'MultiPointShape' to store raw
    -- @['Coord']@ rather than @['Shape']@.
    CompoundCurveShape [CurveSegment]
  | -- | Polygon whose rings may be curved: each ring is a full 'Curve' rather than a plain sequence
    -- of coordinates.
    --
    -- Members are restricted to 'Curve' rather than 'Shape' for the same reason 'CompoundCurveShape'
    -- is restricted to 'CurveSegment': a curve polygon's ring can only be a @LineString@, a
    -- @CircularString@ or a @CompoundCurve@, never a @Polygon@ or anything else.
    CurvePolygonShape [Curve]
  | -- | Heterogeneous collection of curves.
    --
    -- Members are restricted to 'Curve' rather than 'Shape', again for the same reason as
    -- 'CompoundCurveShape': a multi-curve's member can only be a @LineString@, a @CircularString@ or
    -- a @CompoundCurve@, never a @Polygon@ or anything else.
    MultiCurveShape [Curve]
  | -- | Heterogeneous collection of surfaces.
    --
    -- Members are restricted to 'Surface' rather than 'Shape', for the same reason
    -- 'CompoundCurveShape' is restricted to 'CurveSegment': a multi-surface's member can only be a
    -- @Polygon@ or a @CurvePolygon@, never anything else.
    MultiSurfaceShape [Surface]
  deriving stock (Eq, Ord, Show, Read)

-- | Coordinate in one of the four dimensionalities PostGIS supports.
--
-- @Z@ is an elevation, @M@ is a measure — an application-defined value interpolated along the geometry.
data Coord
  = -- | 2D.
    XyCoord
      -- | X ordinate.
      Double
      -- | Y ordinate.
      Double
  | -- | 3D.
    XyzCoord
      -- | X ordinate.
      Double
      -- | Y ordinate.
      Double
      -- | Z ordinate.
      Double
  | -- | 2D with a measure.
    XymCoord
      -- | X ordinate.
      Double
      -- | Y ordinate.
      Double
      -- | M ordinate.
      Double
  | -- | 3D with a measure.
    XyzmCoord
      -- | X ordinate.
      Double
      -- | Y ordinate.
      Double
      -- | Z ordinate.
      Double
      -- | M ordinate.
      Double
  deriving stock (Eq, Ord, Show, Read)

-- | Non-Uniform Rational B-Spline curve, per ISO/IEC 13249-3:2016 (SQL/MM Spatial).
--
-- __Caution:__ untested against any real PostGIS server. See the note on 'Geometry'.
data NurbsCurve
  = NurbsCurve
      -- | Degree.
      Word32
      -- | Control points, each paired with its weight. A weight of @1@ is unweighted.
      [(Coord, Double)]
      -- | Knot vector.
      [Double]
  deriving stock (Eq, Ord, Show, Read)

-- | One continuous curve segment: the building block of a 'CompoundCurveShape'.
--
-- Deliberately does not reuse 'Shape' — see the Haddock on 'CompoundCurveShape' for why.
data CurveSegment
  = -- | Straight segment, wire-compatible with 'LineStringShape'.
    LineSegment [Coord]
  | -- | Circular-arc segment, wire-compatible with 'CircularStringShape'.
    ArcSegment [Coord]
  deriving stock (Eq, Ord, Show, Read)

-- | Any curve: a single segment, or several chained end-to-end (an OGC/SQL-MM @CompoundCurve@).
--
-- The building block of 'CurvePolygonShape'\'s rings and 'MultiCurveShape'\'s members. Deliberately
-- does not reuse 'Shape', for the same reason 'CurveSegment' does not: a ring or member here can
-- only be a @LineString@, a @CircularString@ or a @CompoundCurve@, never a @Polygon@ or anything else.
data Curve
  = -- | Single segment, wire-compatible with 'LineStringShape' or 'CircularStringShape', depending
    -- on the wrapped 'CurveSegment'\'s own constructor.
    SegmentCurve CurveSegment
  | -- | Several segments chained end to end, wire-compatible with 'CompoundCurveShape'.
    ChainedCurve [CurveSegment]
  deriving stock (Eq, Ord, Show, Read)

-- | Any surface: a plain polygon, or one whose rings may be curved (an OGC/SQL-MM @CurvePolygon@).
--
-- The building block of 'MultiSurfaceShape'\'s members. Deliberately does not reuse 'Shape', for the
-- same reason 'Curve' does not: a member here can only be a @Polygon@ or a @CurvePolygon@, never
-- anything else.
data Surface
  = -- | Plain polygon, wire-compatible with 'PolygonShape'.
    PolygonSurface [[Coord]]
  | -- | Polygon with possibly curved rings, wire-compatible with 'CurvePolygonShape'.
    CurvedSurface [Curve]
  deriving stock (Eq, Ord, Show, Read)

instance Hashable Geometry where
  hashWithSalt salt (Geometry srid shape) =
    salt `hashWithSalt` srid `hashWithSalt` shape

instance Hashable Shape where
  hashWithSalt salt = \case
    PointShape coord -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` coord
    LineStringShape coords -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` coords
    PolygonShape rings -> salt `hashWithSalt` (2 :: Int) `hashWithSalt` rings
    MultiPointShape coords -> salt `hashWithSalt` (3 :: Int) `hashWithSalt` coords
    MultiLineStringShape lineStrings -> salt `hashWithSalt` (4 :: Int) `hashWithSalt` lineStrings
    MultiPolygonShape polygons -> salt `hashWithSalt` (5 :: Int) `hashWithSalt` polygons
    GeometryCollectionShape shapes -> salt `hashWithSalt` (6 :: Int) `hashWithSalt` shapes
    CircularStringShape coords -> salt `hashWithSalt` (7 :: Int) `hashWithSalt` coords
    TriangleShape coords -> salt `hashWithSalt` (8 :: Int) `hashWithSalt` coords
    PolyhedralSurfaceShape polygons -> salt `hashWithSalt` (9 :: Int) `hashWithSalt` polygons
    NurbsCurveShape nurbsCurve -> salt `hashWithSalt` (10 :: Int) `hashWithSalt` nurbsCurve
    TinShape triangles -> salt `hashWithSalt` (11 :: Int) `hashWithSalt` triangles
    CompoundCurveShape segments -> salt `hashWithSalt` (12 :: Int) `hashWithSalt` segments
    CurvePolygonShape curves -> salt `hashWithSalt` (13 :: Int) `hashWithSalt` curves
    MultiCurveShape curves -> salt `hashWithSalt` (14 :: Int) `hashWithSalt` curves
    MultiSurfaceShape surfaces -> salt `hashWithSalt` (15 :: Int) `hashWithSalt` surfaces

instance Hashable Coord where
  hashWithSalt salt = \case
    XyCoord x y -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` x `hashWithSalt` y
    XyzCoord x y z -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z
    XymCoord x y m -> salt `hashWithSalt` (2 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` m
    XyzmCoord x y z m -> salt `hashWithSalt` (3 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z `hashWithSalt` m

instance Hashable NurbsCurve where
  hashWithSalt salt (NurbsCurve degree controlPoints knots) =
    salt `hashWithSalt` degree `hashWithSalt` controlPoints `hashWithSalt` knots

instance Hashable CurveSegment where
  hashWithSalt salt = \case
    LineSegment coords -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` coords
    ArcSegment coords -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` coords

instance Hashable Curve where
  hashWithSalt salt = \case
    SegmentCurve segment -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` segment
    ChainedCurve segments -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` segments

instance Hashable Surface where
  hashWithSalt salt = \case
    PolygonSurface rings -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` rings
    CurvedSurface curves -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` curves

instance Arbitrary Geometry where
  arbitrary = do
    dim <- QuickCheck.elements [XyDim, XyzDim, XymDim, XyzmDim]
    shape <- QuickCheck.sized (shapeGen dim)
    srid <- sridGen
    pure (Geometry srid shape)

  shrink (Geometry srid shape) =
    mconcat
      [ mapMaybe (`refineFromShapeAndSrid` srid) (shrinkShape shape),
        mapMaybe (refineFromShapeAndSrid shape) (shrinkSrid srid)
      ]

instance IsPrimitive Geometry where
  schemaName = Tagged Nothing
  typeName = Tagged "geometry"
  baseOid = Tagged Nothing
  arrayOid = Tagged Nothing
  typeParams = Tagged []

  textualEncoder geometry =
    foldMap TextBuilder.hexadecimal (ByteString.unpack (Write.toByteString (binaryEncoder geometry)))

  textualDecoder = do
    hexText <- Attoparsec.takeText
    bytes <- either fail pure (parseHexBytes hexText)
    case PtrPeeker.runVariableOnByteStringWithRemainders (binaryDecoder @Geometry) bytes of
      Left bytesNeeded ->
        fail ("geometry: EWKB payload is short of " <> show bytesNeeded <> " bytes")
      Right (Left err, _) ->
        fail ("geometry: " <> show err)
      Right (Right geometry, remainder)
        | ByteString.null remainder -> pure geometry
        | otherwise ->
            fail ("geometry: " <> show (ByteString.length remainder) <> " bytes left after the EWKB payload")
    where
      parseHexBytes :: Text -> Either String ByteString
      parseHexBytes = fmap ByteString.pack . parseHexPairs . Text.unpack
      parseHexPairs :: [Char] -> Either String [Word8]
      parseHexPairs = \case
        [] -> Right []
        [_] -> Left "Odd number of hexadecimal digits"
        a : b : rest -> (:) <$> parseHexPair a b <*> parseHexPairs rest
      parseHexPair :: Char -> Char -> Either String Word8
      parseHexPair a b = do
        high <- parseHexDigit a
        low <- parseHexDigit b
        pure (high * 16 + low)
      parseHexDigit :: Char -> Either String Word8
      parseHexDigit c
        | c >= '0' && c <= '9' = Right (fromIntegral (ord c - ord '0'))
        | c >= 'a' && c <= 'f' = Right (fromIntegral (ord c - ord 'a' + 10))
        | c >= 'A' && c <= 'F' = Right (fromIntegral (ord c - ord 'A' + 10))
        | otherwise = Left ("Invalid hexadecimal digit: " <> [c])

instance IsBinaryPrimitive Geometry where
  binaryEncoder (Geometry srid shape) =
    -- 'Geometry' is only constructible via 'refineFromShapeAndSrid' and
    -- 'binaryDecoder', both of which reject shape trees whose coordinates
    -- disagree on dimensionality, so 'shapeDim' cannot fail here.
    writeGeometry srid (fromMaybe XyDim (shapeDim shape)) shape

  binaryDecoder = runExceptT do
    (srid, shape) <- readGeometry
    case refineFromShapeAndSrid shape srid of
      Just geometry -> pure geometry
      Nothing ->
        throwError
          ( DecodingError
              ["geometry"]
              ( UnsupportedValueDecodingErrorReason
                  "All coordinates of a geometry must share the same dimensionality"
                  (shapeName shape)
              )
          )

-- * Accessors

-- | Extract the spatial reference identifier, if the geometry carries one.
toSrid :: Geometry -> Maybe Int32
toSrid (Geometry srid _) = srid

-- | Extract the shape.
toShape :: Geometry -> Shape
toShape (Geometry _ shape) = shape

-- * Constructors

-- | Construct a 'Geometry' without an SRID.
--
-- Returns 'Nothing' if the coordinates of the shape tree do not all share the same dimensionality.
refineFromShape :: Shape -> Maybe Geometry
refineFromShape shape = refineFromShapeAndSrid shape Nothing

-- | Construct a 'Geometry' with an optional SRID.
--
-- Returns 'Nothing' if the coordinates of the shape tree do not all share the same dimensionality
-- or if the SRID is not a positive integer. PostGIS normalises @srid <= 0@ to @SRID_UNKNOWN@ (0),
-- which breaks the round-trip: the value would encode with the SRID flag but decode back as
-- 'Nothing', so those values are rejected.
refineFromShapeAndSrid :: Shape -> Maybe Int32 -> Maybe Geometry
refineFromShapeAndSrid shape srid
  | maybe True (0 <) srid = Geometry srid shape <$ shapeDim shape
  | otherwise = Nothing

-- * Dimensionality

-- | Dimensionality shared by all coordinates of a geometry.
data Dim = XyDim | XyzDim | XymDim | XyzmDim
  deriving stock (Eq, Ord, Show)

-- | Dimensionality that every coordinate of the shape tree agrees on, or 'Nothing' if they disagree.
--
-- A tree with no coordinates at all, such as an empty @MultiPoint@, is 'XyDim'.
shapeDim :: Shape -> Maybe Dim
shapeDim shape = fromMaybe XyDim <$> goShape Nothing shape
  where
    goShape :: Maybe Dim -> Shape -> Maybe (Maybe Dim)
    goShape acc = \case
      PointShape coord -> goCoord acc coord
      LineStringShape coords -> goCoords acc coords
      CircularStringShape coords -> goCoords acc coords
      PolygonShape rings -> foldM goCoords acc rings
      MultiPointShape coords -> goCoords acc coords
      MultiLineStringShape lineStrings -> foldM goCoords acc lineStrings
      MultiPolygonShape polygons -> foldM (foldM goCoords) acc polygons
      PolyhedralSurfaceShape polygons -> foldM (foldM goCoords) acc polygons
      GeometryCollectionShape shapes -> foldM goShape acc shapes
      TriangleShape coords -> goCoords acc coords
      NurbsCurveShape (NurbsCurve _ controlPoints _) -> goCoords acc (fst <$> controlPoints)
      TinShape triangles -> foldM goCoords acc triangles
      CompoundCurveShape segments -> foldM goSegment acc segments
      CurvePolygonShape curves -> foldM goCurve acc curves
      MultiCurveShape curves -> foldM goCurve acc curves
      MultiSurfaceShape surfaces -> foldM goSurface acc surfaces
    goSegment :: Maybe Dim -> CurveSegment -> Maybe (Maybe Dim)
    goSegment acc = \case
      LineSegment coords -> goCoords acc coords
      ArcSegment coords -> goCoords acc coords
    goCurve :: Maybe Dim -> Curve -> Maybe (Maybe Dim)
    goCurve acc = \case
      SegmentCurve segment -> goSegment acc segment
      ChainedCurve segments -> foldM goSegment acc segments
    goSurface :: Maybe Dim -> Surface -> Maybe (Maybe Dim)
    goSurface acc = \case
      PolygonSurface rings -> foldM goCoords acc rings
      CurvedSurface curves -> foldM goCurve acc curves
    goCoords :: Maybe Dim -> [Coord] -> Maybe (Maybe Dim)
    goCoords = foldM goCoord
    goCoord :: Maybe Dim -> Coord -> Maybe (Maybe Dim)
    goCoord acc coord =
      let dim = coordDim coord
       in case acc of
            Nothing -> Just (Just dim)
            Just acc' -> if acc' == dim then Just (Just dim) else Nothing

coordDim :: Coord -> Dim
coordDim = \case
  XyCoord {} -> XyDim
  XyzCoord {} -> XyzDim
  XymCoord {} -> XymDim
  XyzmCoord {} -> XyzmDim

-- * EWKB header

-- | Byte order of an EWKB payload, as signalled by its leading marker byte.
data ByteOrder
  = -- | XDR. Marker @0@.
    BigEndianByteOrder
  | -- | NDR. Marker @1@.
    LittleEndianByteOrder

-- | OGC type codes, as they appear in the low bits of the EWKB type header.
pointTypeCode, lineStringTypeCode, polygonTypeCode, multiPointTypeCode, multiLineStringTypeCode, multiPolygonTypeCode, geometryCollectionTypeCode, circularStringTypeCode, triangleTypeCode, polyhedralSurfaceTypeCode, tinTypeCode, compoundCurveTypeCode, curvePolygonTypeCode, multiCurveTypeCode, multiSurfaceTypeCode :: Word32
pointTypeCode = 1
lineStringTypeCode = 2
polygonTypeCode = 3
multiPointTypeCode = 4
multiLineStringTypeCode = 5
multiPolygonTypeCode = 6
geometryCollectionTypeCode = 7
circularStringTypeCode = 8
compoundCurveTypeCode = 9
curvePolygonTypeCode = 10
multiCurveTypeCode = 11
multiSurfaceTypeCode = 12
triangleTypeCode = 17
polyhedralSurfaceTypeCode = 15
tinTypeCode = 16

-- | ISO/IEC 13249-3 (SQL/MM) type code for a NURBS curve.
--
-- Unlike the seven OGC shapes above, PostGIS's WKB writer signals a NURBS curve's dimensionality
-- through this code's magnitude rather than through 'zFlag'\/'mFlag': the wire byte is
-- @nurbsCurveBaseTypeCode + isoDimOffset dim@, e.g. @1021@ for an XYZ curve. See 'isoDimOffset'.
nurbsCurveBaseTypeCode :: Word32
nurbsCurveBaseTypeCode = 21

-- | Dimensionality offset that PostGIS's WKB writer adds to 'nurbsCurveBaseTypeCode', per the
-- ISO/IEC 13249-3 (SQL/MM) extended-WKB numbering scheme: plain 2D types occupy 1-999, @+1000@
-- selects the Z variant, @+2000@ the M variant and @+3000@ the ZM variant.
--
-- __Assumption, not verified against a released PostGIS:__ NURBS curve support only exists on
-- PostGIS's development branch (merged June 2026) and no released version speaks this format, so
-- this can't be checked against real server output. The @+1000@\/@+2000@\/@+3000@ offsets are given
-- directly by the ticket that specified this feature, and the "no offset means 2D" reading follows
-- the same SQL/MM scheme PostGIS already uses for its other ISO curve\/surface type codes (e.g.
-- @CircularString@ = 8, @CompoundCurve@ = 9, none of which this codebase implements) — those, too,
-- carry no offset for their bare 2D form. If PostGIS's actual NURBS implementation turns out to
-- reserve 0 for something other than XY, only this function and 'isoOffsetToDim' need to change.
isoDimOffset :: Dim -> Word32
isoDimOffset = \case
  XyDim -> 0
  XyzDim -> 1000
  XymDim -> 2000
  XyzmDim -> 3000

-- | Inverse of 'isoDimOffset'. 'Nothing' for any offset PostGIS's NURBS writer is not documented to emit.
isoOffsetToDim :: Word32 -> Maybe Dim
isoOffsetToDim = \case
  0 -> Just XyDim
  1 -> Just XyzDim
  2 -> Just XymDim
  3 -> Just XyzmDim
  _ -> Nothing

-- | Flag bits of the EWKB type header.
zFlag, mFlag, sridFlag :: Word32
zFlag = 0x80000000
mFlag = 0x40000000
sridFlag = 0x20000000

-- | Mask that liblwgeom applies to separate the OGC type code from the EWKB flags.
--
-- The four top bits are the @Z@, @M@, @SRID@ and bounding-box markers. PostGIS never emits the
-- bounding-box one in WKB, but it is masked off all the same, exactly as @lwtype_from_wkb_state@ does.
typeCodeMask :: Word32
typeCodeMask = 0x0FFFFFFF

dimToFlags :: Dim -> Word32
dimToFlags = \case
  XyDim -> 0
  XyzDim -> zFlag
  XymDim -> mFlag
  XyzmDim -> zFlag .|. mFlag

shapeToTypeCode :: Shape -> Word32
shapeToTypeCode = \case
  PointShape {} -> pointTypeCode
  LineStringShape {} -> lineStringTypeCode
  PolygonShape {} -> polygonTypeCode
  MultiPointShape {} -> multiPointTypeCode
  MultiLineStringShape {} -> multiLineStringTypeCode
  MultiPolygonShape {} -> multiPolygonTypeCode
  PolyhedralSurfaceShape {} -> polyhedralSurfaceTypeCode
  GeometryCollectionShape {} -> geometryCollectionTypeCode
  CircularStringShape {} -> circularStringTypeCode
  TriangleShape {} -> triangleTypeCode
  NurbsCurveShape {} -> nurbsCurveBaseTypeCode
  TinShape {} -> tinTypeCode
  CompoundCurveShape {} -> compoundCurveTypeCode
  CurvePolygonShape {} -> curvePolygonTypeCode
  MultiCurveShape {} -> multiCurveTypeCode
  MultiSurfaceShape {} -> multiSurfaceTypeCode

-- | Name of the shape in the OGC vocabulary. Used for reporting decoding errors.
shapeName :: Shape -> Text
shapeName = \case
  PointShape {} -> "Point"
  LineStringShape {} -> "LineString"
  PolygonShape {} -> "Polygon"
  MultiPointShape {} -> "MultiPoint"
  MultiLineStringShape {} -> "MultiLineString"
  MultiPolygonShape {} -> "MultiPolygon"
  PolyhedralSurfaceShape {} -> "PolyhedralSurface"
  GeometryCollectionShape {} -> "GeometryCollection"
  CircularStringShape {} -> "CircularString"
  TriangleShape {} -> "Triangle"
  NurbsCurveShape {} -> "NurbsCurve"
  TinShape {} -> "TIN"
  CompoundCurveShape {} -> "CompoundCurve"
  CurvePolygonShape {} -> "CurvePolygon"
  MultiCurveShape {} -> "MultiCurve"
  MultiSurfaceShape {} -> "MultiSurface"

-- * Binary encoder

-- | Encode a geometry as EWKB, in little-endian byte order.
--
-- The SRID flag and field are only emitted when an SRID is given. PostGIS stores the SRID on the outer
-- geometry alone, so the sub-geometries of the @Multi@ and collection shapes are written without one.
writeGeometry :: Maybe Int32 -> Dim -> Shape -> Write.Write
writeGeometry srid dim shape =
  mconcat
    [ Write.word8 1,
      Write.lWord32 (typeCode .|. maybe 0 (const sridFlag) srid),
      foldMap (Write.lWord32 . fromIntegral) srid,
      writeShape dim shape
    ]
  where
    -- A NURBS curve is the one shape whose dimensionality PostGIS's writer bakes into the type
    -- code's magnitude (see 'isoDimOffset') instead of the Z\/M flag bits every other shape uses.
    -- The SRID flag above is unaffected either way: it is a separate bit that both schemes share.
    typeCode = case shape of
      NurbsCurveShape {} -> shapeToTypeCode shape + isoDimOffset dim
      _ -> shapeToTypeCode shape .|. dimToFlags dim

writeShape :: Dim -> Shape -> Write.Write
writeShape dim = \case
  PointShape coord -> writeCoord coord
  LineStringShape coords -> writeCoords coords
  CircularStringShape coords -> writeCoords coords
  PolygonShape rings -> writeCount rings <> foldMap writeCoords rings
  MultiPointShape coords -> writeCount coords <> foldMap (writeSubGeometry . PointShape) coords
  MultiLineStringShape lineStrings -> writeCount lineStrings <> foldMap (writeSubGeometry . LineStringShape) lineStrings
  MultiPolygonShape polygons -> writeCount polygons <> foldMap (writeSubGeometry . PolygonShape) polygons
  PolyhedralSurfaceShape polygons -> writeCount polygons <> foldMap (writeSubGeometry . PolygonShape) polygons
  GeometryCollectionShape shapes -> writeCount shapes <> foldMap writeSubGeometry shapes
  TriangleShape coords -> case coords of
    [] -> Write.lWord32 0
    _ -> Write.lWord32 1 <> writeCoords coords
  NurbsCurveShape nurbsCurve -> writeNurbsCurve nurbsCurve
  TinShape triangles -> writeCount triangles <> foldMap (writeSubGeometry . TriangleShape) triangles
  CompoundCurveShape segments -> writeCount segments <> foldMap writeSegment segments
  CurvePolygonShape curves -> writeCount curves <> foldMap writeCurve curves
  MultiCurveShape curves -> writeCount curves <> foldMap writeCurve curves
  MultiSurfaceShape surfaces -> writeCount surfaces <> foldMap writeSurface surfaces
  where
    writeCount :: [a] -> Write.Write
    writeCount = Write.lWord32 . fromIntegral . length
    writeCoords :: [Coord] -> Write.Write
    writeCoords coords = writeCount coords <> foldMap writeCoord coords
    writeSubGeometry :: Shape -> Write.Write
    writeSubGeometry = writeGeometry Nothing dim
    writeSegment :: CurveSegment -> Write.Write
    writeSegment = \case
      LineSegment coords -> writeSubGeometry (LineStringShape coords)
      ArcSegment coords -> writeSubGeometry (CircularStringShape coords)
    writeCurve :: Curve -> Write.Write
    writeCurve = \case
      SegmentCurve segment -> writeSegment segment
      ChainedCurve segments -> writeSubGeometry (CompoundCurveShape segments)
    writeSurface :: Surface -> Write.Write
    writeSurface = \case
      PolygonSurface rings -> writeSubGeometry (PolygonShape rings)
      CurvedSurface curves -> writeSubGeometry (CurvePolygonShape curves)

-- | Encode the ordinates of a coordinate.
--
-- Which of them are present is determined by the coordinate's own constructor, which the 'Geometry'
-- invariant keeps in agreement with the dimensionality announced in the header.
writeCoord :: Coord -> Write.Write
writeCoord = \case
  XyCoord x y -> writeDouble x <> writeDouble y
  XyzCoord x y z -> writeDouble x <> writeDouble y <> writeDouble z
  XymCoord x y m -> writeDouble x <> writeDouble y <> writeDouble m
  XyzmCoord x y z m -> writeDouble x <> writeDouble y <> writeDouble z <> writeDouble m
  where
    writeDouble = Write.lWord64 . castDoubleToWord64

-- | Encode a NURBS curve's body: degree, then the count-prefixed control points, then the
-- count-prefixed knot vector. Mirrors 'readNurbsCurve'.
writeNurbsCurve :: NurbsCurve -> Write.Write
writeNurbsCurve (NurbsCurve degree controlPoints knots) =
  mconcat
    [ Write.lWord32 degree,
      Write.lWord32 (fromIntegral (length controlPoints)),
      foldMap writeNurbsControlPoint controlPoints,
      Write.lWord32 (fromIntegral (length knots)),
      foldMap writeDouble knots
    ]
  where
    writeDouble = Write.lWord64 . castDoubleToWord64

-- | Encode one control point: its own (redundant, but ISO-mandated) byte-order marker — always NDR,
-- matching the rest of this encoder — then its coordinates, then the weight flag byte. A weight of
-- exactly @1.0@ is the default and is written as flag @0@ with the weight omitted; any other value,
-- including @0.0@, is written explicitly as flag @1@ followed by the weight.
writeNurbsControlPoint :: (Coord, Double) -> Write.Write
writeNurbsControlPoint (coord, weight) =
  mconcat
    [ Write.word8 1,
      writeCoord coord,
      if weight == 1.0
        then Write.word8 0
        else Write.word8 1 <> Write.lWord64 (castDoubleToWord64 weight)
    ]

-- * Binary decoder

-- | Decode an EWKB geometry, yielding its SRID and shape.
--
-- Sub-geometries are decoded through the same function: EWKB lets them carry an SRID field of their
-- own, which has to be consumed to keep the stream aligned, but PostGIS ignores its value in favour
-- of the one of the enclosing geometry. The callers below accordingly discard it.
readGeometry :: ExceptT DecodingError PtrPeeker.Variable (Maybe Int32, Shape)
readGeometry = do
  byteOrderMarker <- lift (PtrPeeker.fixed PtrPeeker.unsignedInt1)
  byteOrder <- case byteOrderMarker of
    0 -> pure BigEndianByteOrder
    1 -> pure LittleEndianByteOrder
    _ ->
      throwError
        ( DecodingError
            ["byte-order-marker"]
            (UnexpectedValueDecodingErrorReason "0 or 1" (TextBuilder.toText (TextBuilder.decimal byteOrderMarker)))
        )
  typeWithFlags <- lift (readWord32 byteOrder)
  let maskedTypeCode = typeWithFlags .&. typeCodeMask
  -- A NURBS curve is the one shape whose dimensionality is not carried in the Z/M flag bits
  -- (testBit 31 / testBit 30): PostGIS's writer instead bakes it into the type code's magnitude
  -- (see 'isoDimOffset'). Detect that case first, from the masked type code alone, before falling
  -- back to the ordinary flag-bit interpretation every other shape uses.
  (dim, typeCode) <-
    if maskedTypeCode `mod` 1000 == nurbsCurveBaseTypeCode
      then case isoOffsetToDim (maskedTypeCode `div` 1000) of
        Just isoDim -> pure (isoDim, nurbsCurveBaseTypeCode)
        Nothing ->
          throwError
            ( DecodingError
                ["type-code"]
                ( UnsupportedValueDecodingErrorReason
                    "Unknown NURBS curve ISO dimensionality offset"
                    (TextBuilder.toText (TextBuilder.decimal maskedTypeCode))
                )
            )
      else
        pure
          ( case (testBit typeWithFlags 31, testBit typeWithFlags 30) of
              (False, False) -> XyDim
              (True, False) -> XyzDim
              (False, True) -> XymDim
              (True, True) -> XyzmDim,
            maskedTypeCode
          )
  srid <-
    if testBit typeWithFlags 29
      then Just . fromIntegral <$> lift (readWord32 byteOrder)
      else pure Nothing
  shape <- readShape byteOrder dim typeCode
  pure (srid, shape)

readShape :: ByteOrder -> Dim -> Word32 -> ExceptT DecodingError PtrPeeker.Variable Shape
readShape byteOrder dim typeCode
  | typeCode == pointTypeCode =
      PointShape <$> lift (readCoord byteOrder dim)
  | typeCode == lineStringTypeCode =
      LineStringShape <$> lift readCoords
  | typeCode == circularStringTypeCode =
      CircularStringShape <$> lift readCoords
  | typeCode == polygonTypeCode =
      PolygonShape <$> lift (readSequence readCoords)
  | typeCode == multiPointTypeCode =
      MultiPointShape <$> readSubShapes "MultiPoint" "Point" \case
        PointShape coord -> Just coord
        _ -> Nothing
  | typeCode == multiLineStringTypeCode =
      MultiLineStringShape <$> readSubShapes "MultiLineString" "LineString" \case
        LineStringShape coords -> Just coords
        _ -> Nothing
  | typeCode == multiPolygonTypeCode =
      MultiPolygonShape <$> readSubShapes "MultiPolygon" "Polygon" \case
        PolygonShape rings -> Just rings
        _ -> Nothing
  | typeCode == polyhedralSurfaceTypeCode =
      PolyhedralSurfaceShape <$> readSubShapes "PolyhedralSurface" "Polygon" \case
        PolygonShape rings -> Just rings
        _ -> Nothing
  | typeCode == geometryCollectionTypeCode = do
      count <- lift (readWord32 byteOrder)
      GeometryCollectionShape <$> replicateM (fromIntegral count) (snd <$> readGeometry)
  | typeCode == triangleTypeCode = do
      ringCount <- lift (readWord32 byteOrder)
      case ringCount of
        0 -> pure (TriangleShape [])
        1 -> TriangleShape <$> lift readCoords
        _ ->
          throwError
            ( DecodingError
                ["Triangle"]
                (UnexpectedValueDecodingErrorReason "0 or 1 rings" (TextBuilder.toText (TextBuilder.decimal ringCount)))
            )
  | typeCode == nurbsCurveBaseTypeCode =
      NurbsCurveShape <$> readNurbsCurve byteOrder dim
  | typeCode == tinTypeCode =
      TinShape <$> readSubShapes "TIN" "Triangle" \case
        TriangleShape coords -> Just coords
        _ -> Nothing
  | typeCode == compoundCurveTypeCode =
      CompoundCurveShape <$> readSubShapes "CompoundCurve" "LineString or CircularString" \case
        LineStringShape coords -> Just (LineSegment coords)
        CircularStringShape coords -> Just (ArcSegment coords)
        _ -> Nothing
  | typeCode == curvePolygonTypeCode =
      CurvePolygonShape <$> readSubShapes "CurvePolygon" "LineString, CircularString or CompoundCurve" readCurveProjection
  | typeCode == multiCurveTypeCode =
      MultiCurveShape <$> readSubShapes "MultiCurve" "LineString, CircularString or CompoundCurve" readCurveProjection
  | typeCode == multiSurfaceTypeCode =
      MultiSurfaceShape <$> readSubShapes "MultiSurface" "Polygon or CurvePolygon" readSurfaceProjection
  | otherwise =
      throwError
        ( DecodingError
            ["type-code"]
            (UnsupportedValueDecodingErrorReason "Unknown geometry type code" (TextBuilder.toText (TextBuilder.decimal typeCode)))
        )
  where
    readCoords :: PtrPeeker.Variable [Coord]
    readCoords = readSequence (readCoord byteOrder dim)
    -- A count-prefixed sequence of elements.
    readSequence :: PtrPeeker.Variable a -> PtrPeeker.Variable [a]
    readSequence element = do
      count <- readWord32 byteOrder
      replicateM (fromIntegral count) element
    -- A count-prefixed sequence of sub-geometries, each of which is required to be of the shape
    -- that the given projection accepts. The arguments are the name of the shape being decoded and
    -- the name of the sub-shape, both for reporting errors.
    readSubShapes ::
      Text ->
      Text ->
      (Shape -> Maybe a) ->
      ExceptT DecodingError PtrPeeker.Variable [a]
    readSubShapes name subShapeName project = do
      count <- lift (readWord32 byteOrder)
      replicateM (fromIntegral count) do
        shape <- snd <$> readGeometry
        case project shape of
          Just projection -> pure projection
          Nothing ->
            throwError
              ( DecodingError
                  [name]
                  (UnexpectedValueDecodingErrorReason subShapeName (shapeName shape))
              )
    -- Projection shared by 'CurvePolygonShape' and 'MultiCurveShape': both admit the same three
    -- shapes as a ring\/member, wire-compatible with 'Curve'\'s own two constructors.
    readCurveProjection :: Shape -> Maybe Curve
    readCurveProjection = \case
      LineStringShape coords -> Just (SegmentCurve (LineSegment coords))
      CircularStringShape coords -> Just (SegmentCurve (ArcSegment coords))
      CompoundCurveShape segments -> Just (ChainedCurve segments)
      _ -> Nothing
    -- Projection for 'MultiSurfaceShape': a member is either a plain 'PolygonShape' or a
    -- 'CurvePolygonShape', wire-compatible with 'Surface'\'s own two constructors.
    readSurfaceProjection :: Shape -> Maybe Surface
    readSurfaceProjection = \case
      PolygonShape rings -> Just (PolygonSurface rings)
      CurvePolygonShape curves -> Just (CurvedSurface curves)
      _ -> Nothing

readCoord :: ByteOrder -> Dim -> PtrPeeker.Variable Coord
readCoord byteOrder = \case
  XyDim -> XyCoord <$> readDouble <*> readDouble
  XyzDim -> XyzCoord <$> readDouble <*> readDouble <*> readDouble
  XymDim -> XymCoord <$> readDouble <*> readDouble <*> readDouble
  XyzmDim -> XyzmCoord <$> readDouble <*> readDouble <*> readDouble <*> readDouble
  where
    readDouble = castWord64ToDouble <$> readWord64 byteOrder

-- | Decode a NURBS curve's body: degree, then the count-prefixed control points, then the
-- count-prefixed knot vector. Mirrors 'writeNurbsCurve'.
--
-- No arithmetic relationship between the degree, control point count and knot count is checked:
-- as elsewhere in this codebase, already-server-validated structure is trusted rather than
-- re-validated.
readNurbsCurve :: ByteOrder -> Dim -> ExceptT DecodingError PtrPeeker.Variable NurbsCurve
readNurbsCurve byteOrder dim = do
  degree <- lift (readWord32 byteOrder)
  pointCount <- lift (readWord32 byteOrder)
  controlPoints <- replicateM (fromIntegral pointCount) (readNurbsControlPoint dim)
  knotCount <- lift (readWord32 byteOrder)
  knots <- lift (replicateM (fromIntegral knotCount) readDouble)
  pure (NurbsCurve degree controlPoints knots)
  where
    readDouble = castWord64ToDouble <$> readWord64 byteOrder

-- | Decode one control point: its own (redundant, but ISO-mandated) byte-order marker, then its
-- coordinates in that byte order, then a weight flag byte — @0@ means the default weight @1.0@
-- (omitted on the wire), @1@ means an explicit weight follows.
readNurbsControlPoint :: Dim -> ExceptT DecodingError PtrPeeker.Variable (Coord, Double)
readNurbsControlPoint dim = do
  pointByteOrderMarker <- lift (PtrPeeker.fixed PtrPeeker.unsignedInt1)
  pointByteOrder <- case pointByteOrderMarker of
    0 -> pure BigEndianByteOrder
    1 -> pure LittleEndianByteOrder
    _ ->
      throwError
        ( DecodingError
            ["nurbs-curve", "control-point", "byte-order-marker"]
            (UnexpectedValueDecodingErrorReason "0 or 1" (TextBuilder.toText (TextBuilder.decimal pointByteOrderMarker)))
        )
  coord <- lift (readCoord pointByteOrder dim)
  hasWeight <- lift (PtrPeeker.fixed PtrPeeker.unsignedInt1)
  weight <- case hasWeight of
    0 -> pure 1.0
    _ -> lift (castWord64ToDouble <$> readWord64 pointByteOrder)
  pure (coord, weight)

readWord32 :: ByteOrder -> PtrPeeker.Variable Word32
readWord32 = \case
  BigEndianByteOrder -> PtrPeeker.fixed PtrPeeker.beUnsignedInt4
  LittleEndianByteOrder -> PtrPeeker.fixed PtrPeeker.leUnsignedInt4

readWord64 :: ByteOrder -> PtrPeeker.Variable Word64
readWord64 = \case
  BigEndianByteOrder -> PtrPeeker.fixed PtrPeeker.beUnsignedInt8
  LittleEndianByteOrder -> PtrPeeker.fixed PtrPeeker.leUnsignedInt8

-- * QuickCheck generators

-- | Generate an SRID within PostGIS's user-assignable range.
--
-- Values outside of it do not survive a roundtrip: @0@ means \"no SRID\" and loses the flag on output,
-- and anything above @SRID_MAXIMUM@ (999999) gets remapped modulo into the range above
-- @SRID_USER_MAXIMUM@ that PostGIS reserves for itself. @[1, 998999]@ is also where the real EPSG and
-- @spatial_ref_sys@ codes live.
sridGen :: QuickCheck.Gen (Maybe Int32)
sridGen = QuickCheck.oneof [pure Nothing, Just <$> QuickCheck.choose (1, 998_999)]

shrinkSrid :: Maybe Int32 -> [Maybe Int32]
shrinkSrid = filter (maybe True (\srid -> srid >= 1 && srid <= 998_999)) . shrink

-- | Generate a shape whose every coordinate is of the given dimensionality and which satisfies the
-- structural constraints OGC places on it, so that PostGIS accepts it.
--
-- __'NurbsCurveShape' is deliberately never generated here.__ NURBS curve support is not in any
-- released PostGIS version (see the note on 'Geometry'), and the real-server integration test
-- suite draws its 'Geometry' values from this generator; producing one here would break that suite
-- against a server that cannot parse it. Do not add it without first revisiting that risk.
shapeGen :: Dim -> Int -> QuickCheck.Gen Shape
shapeGen dim size =
  QuickCheck.oneof (if size <= 1 then simple else simple <> compound)
  where
    simple =
      [ PointShape <$> coordGen dim,
        LineStringShape <$> lineStringCoordsGen dim,
        CircularStringShape <$> circularStringCoordsGen dim,
        PolygonShape . pure <$> ringCoordsGen dim,
        MultiPointShape <$> QuickCheck.listOf (coordGen dim),
        QuickCheck.oneof [pure (TriangleShape []), TriangleShape <$> ringCoordsGen dim]
      ]
    compound =
      [ MultiLineStringShape <$> QuickCheck.listOf (lineStringCoordsGen dim),
        MultiPolygonShape <$> QuickCheck.listOf (QuickCheck.listOf1 (ringCoordsGen dim)),
        PolyhedralSurfaceShape <$> QuickCheck.listOf (QuickCheck.listOf1 (ringCoordsGen dim)),
        TinShape <$> QuickCheck.listOf (QuickCheck.oneof [pure [], ringCoordsGen dim]),
        CompoundCurveShape <$> QuickCheck.listOf (segmentGen dim),
        CurvePolygonShape <$> QuickCheck.listOf (curveGen dim),
        MultiCurveShape <$> QuickCheck.listOf (curveGen dim),
        MultiSurfaceShape <$> QuickCheck.listOf (surfaceGen dim),
        GeometryCollectionShape <$> QuickCheck.resize subSize (QuickCheck.listOf (shapeGen dim subSize))
      ]
    subSize = div size 4

-- | Generate the coordinates of a line string, of which OGC requires at least two.
lineStringCoordsGen :: Dim -> QuickCheck.Gen [Coord]
lineStringCoordsGen dim = do
  extra <- QuickCheck.choose (0, 6 :: Int)
  QuickCheck.vectorOf (2 + extra) (coordGen dim)

-- | Generate the coordinates of a circular string. PostGIS's @geometry_recv@ enforces
-- @LW_PARSER_CHECK_MINPOINTS | LW_PARSER_CHECK_ODD@ for circular strings: an odd number of
-- coordinates, at least three, each consecutive triple describing one circular arc.
circularStringCoordsGen :: Dim -> QuickCheck.Gen [Coord]
circularStringCoordsGen dim = do
  extraArcs <- QuickCheck.choose (0, 3 :: Int)
  QuickCheck.vectorOf (3 + 2 * extraArcs) (coordGen dim)

-- | Generate the coordinates of a linear ring, which OGC requires to be closed and to consist of at
-- least four coordinates: three distinct ones plus the repetition of the first that closes it.
ringCoordsGen :: Dim -> QuickCheck.Gen [Coord]
ringCoordsGen dim = do
  extra <- QuickCheck.choose (0, 5 :: Int)
  firstCoord <- coordGen dim
  restCoords <- QuickCheck.vectorOf (2 + extra) (coordGen dim)
  pure (firstCoord : restCoords <> [firstCoord])

coordGen :: Dim -> QuickCheck.Gen Coord
coordGen = \case
  XyDim -> XyCoord <$> arbitrary <*> arbitrary
  XyzDim -> XyzCoord <$> arbitrary <*> arbitrary <*> arbitrary
  XymDim -> XymCoord <$> arbitrary <*> arbitrary <*> arbitrary
  XyzmDim -> XyzmCoord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Generate one curve segment, matching 'CurveSegment'\'s own two constructors.
segmentGen :: Dim -> QuickCheck.Gen CurveSegment
segmentGen dim =
  QuickCheck.oneof
    [ LineSegment <$> lineStringCoordsGen dim,
      ArcSegment <$> circularStringCoordsGen dim
    ]

-- | Generate a curve: either a single segment or several chained end to end, matching 'Curve'\'s own
-- two constructors.
curveGen :: Dim -> QuickCheck.Gen Curve
curveGen dim =
  QuickCheck.oneof
    [ SegmentCurve <$> segmentGen dim,
      ChainedCurve <$> QuickCheck.listOf (segmentGen dim)
    ]

-- | Generate a surface, matching 'Surface'\'s own two constructors. A plain polygon surface has at
-- least one ring, matching 'PolygonShape'\'s own generator.
surfaceGen :: Dim -> QuickCheck.Gen Surface
surfaceGen dim =
  QuickCheck.oneof
    [ PolygonSurface . pure <$> ringCoordsGen dim,
      CurvedSurface <$> QuickCheck.listOf (curveGen dim)
    ]

-- | Shrink a shape by dropping members of its collections.
--
-- Individual coordinates are never dropped or shrunk: that would either change the dimensionality of
-- the tree or break the closure of a ring, producing a shape PostGIS rejects.
shrinkShape :: Shape -> [Shape]
shrinkShape = \case
  PointShape _ -> []
  LineStringShape coords ->
    LineStringShape <$> filter ((>= 2) . length) (shrinkMembers coords)
  CircularStringShape coords ->
    CircularStringShape <$> filter (\cs -> length cs >= 3 && odd (length cs)) (shrinkMembers coords)
  PolygonShape rings ->
    PolygonShape <$> filter (not . null) (shrinkMembers rings)
  MultiPointShape coords ->
    MultiPointShape <$> shrinkMembers coords
  MultiLineStringShape lineStrings ->
    MultiLineStringShape <$> shrinkMembers lineStrings
  MultiPolygonShape polygons ->
    MultiPolygonShape <$> shrinkMembers polygons
  PolyhedralSurfaceShape polygons ->
    PolyhedralSurfaceShape <$> shrinkMembers polygons
  GeometryCollectionShape shapes ->
    GeometryCollectionShape <$> QuickCheck.shrinkList shrinkShape shapes
  TriangleShape coords ->
    [TriangleShape [] | not (null coords)]
  TinShape triangles ->
    TinShape <$> shrinkMembers triangles
  -- 'shapeGen' never produces this constructor (see the comment there), so this is never called
  -- with one in practice. Covered only so the pattern match above stays exhaustive.
  NurbsCurveShape _ -> []
  CompoundCurveShape segments ->
    -- Members are dropped wholesale, never shrunk internally: shrinking a segment's own coordinate
    -- list risks breaking the odd-≥3 invariant an 'ArcSegment' requires, the same reason
    -- 'CircularStringShape' does not delegate to a plain 'shrinkMembers' either.
    CompoundCurveShape <$> shrinkMembers segments
  CurvePolygonShape curves ->
    -- Same reasoning as 'CompoundCurveShape': members are dropped wholesale, never shrunk internally.
    CurvePolygonShape <$> shrinkMembers curves
  MultiCurveShape curves ->
    MultiCurveShape <$> shrinkMembers curves
  MultiSurfaceShape surfaces ->
    -- Same reasoning as 'MultiCurveShape': members are dropped wholesale, never shrunk internally.
    MultiSurfaceShape <$> shrinkMembers surfaces
  where
    shrinkMembers :: [a] -> [[a]]
    shrinkMembers = QuickCheck.shrinkList (const [])
