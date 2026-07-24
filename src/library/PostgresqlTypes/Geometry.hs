module PostgresqlTypes.Geometry
  ( Geometry,
    Shape (..),
    Coord (..),

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
-- [PostGIS docs](https://postgis.net/docs/geometry.html).
data Geometry
  = Geometry
      -- | Spatial reference identifier.
      (Maybe Int32)
      -- | Shape.
      Shape
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Geometry)

-- | One of the eight OGC shapes that a 'Geometry' can hold.
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
  | -- | Heterogeneous collection of shapes.
    --
    -- Members do not carry their own SRID: they inherit the one of the enclosing 'Geometry'.
    GeometryCollectionShape [Shape]
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

instance Hashable Coord where
  hashWithSalt salt = \case
    XyCoord x y -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` x `hashWithSalt` y
    XyzCoord x y z -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z
    XymCoord x y m -> salt `hashWithSalt` (2 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` m
    XyzmCoord x y z m -> salt `hashWithSalt` (3 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z `hashWithSalt` m

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

instance IsScalar Geometry where
  schemaName = Tagged Nothing
  typeName = Tagged "geometry"
  baseOid = Tagged Nothing
  arrayOid = Tagged Nothing
  typeParams = Tagged []

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
      GeometryCollectionShape shapes -> foldM goShape acc shapes
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
pointTypeCode, lineStringTypeCode, polygonTypeCode, multiPointTypeCode, multiLineStringTypeCode, multiPolygonTypeCode, geometryCollectionTypeCode, circularStringTypeCode :: Word32
pointTypeCode = 1
lineStringTypeCode = 2
polygonTypeCode = 3
multiPointTypeCode = 4
multiLineStringTypeCode = 5
multiPolygonTypeCode = 6
geometryCollectionTypeCode = 7
circularStringTypeCode = 8

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
  GeometryCollectionShape {} -> geometryCollectionTypeCode
  CircularStringShape {} -> circularStringTypeCode

-- | Name of the shape in the OGC vocabulary. Used for reporting decoding errors.
shapeName :: Shape -> Text
shapeName = \case
  PointShape {} -> "Point"
  LineStringShape {} -> "LineString"
  PolygonShape {} -> "Polygon"
  MultiPointShape {} -> "MultiPoint"
  MultiLineStringShape {} -> "MultiLineString"
  MultiPolygonShape {} -> "MultiPolygon"
  GeometryCollectionShape {} -> "GeometryCollection"
  CircularStringShape {} -> "CircularString"

-- * Binary encoder

-- | Encode a geometry as EWKB, in little-endian byte order.
--
-- The SRID flag and field are only emitted when an SRID is given. PostGIS stores the SRID on the outer
-- geometry alone, so the sub-geometries of the @Multi@ and collection shapes are written without one.
writeGeometry :: Maybe Int32 -> Dim -> Shape -> Write.Write
writeGeometry srid dim shape =
  mconcat
    [ Write.word8 1,
      Write.lWord32 (shapeToTypeCode shape .|. dimToFlags dim .|. maybe 0 (const sridFlag) srid),
      foldMap (Write.lWord32 . fromIntegral) srid,
      writeShape dim shape
    ]

writeShape :: Dim -> Shape -> Write.Write
writeShape dim = \case
  PointShape coord -> writeCoord coord
  LineStringShape coords -> writeCoords coords
  CircularStringShape coords -> writeCoords coords
  PolygonShape rings -> writeCount rings <> foldMap writeCoords rings
  MultiPointShape coords -> writeCount coords <> foldMap (writeSubGeometry . PointShape) coords
  MultiLineStringShape lineStrings -> writeCount lineStrings <> foldMap (writeSubGeometry . LineStringShape) lineStrings
  MultiPolygonShape polygons -> writeCount polygons <> foldMap (writeSubGeometry . PolygonShape) polygons
  GeometryCollectionShape shapes -> writeCount shapes <> foldMap writeSubGeometry shapes
  where
    writeCount :: [a] -> Write.Write
    writeCount = Write.lWord32 . fromIntegral . length
    writeCoords :: [Coord] -> Write.Write
    writeCoords coords = writeCount coords <> foldMap writeCoord coords
    writeSubGeometry :: Shape -> Write.Write
    writeSubGeometry = writeGeometry Nothing dim

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
  let dim = case (testBit typeWithFlags 31, testBit typeWithFlags 30) of
        (False, False) -> XyDim
        (True, False) -> XyzDim
        (False, True) -> XymDim
        (True, True) -> XyzmDim
  srid <-
    if testBit typeWithFlags 29
      then Just . fromIntegral <$> lift (readWord32 byteOrder)
      else pure Nothing
  shape <- readShape byteOrder dim (typeWithFlags .&. typeCodeMask)
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
  | typeCode == geometryCollectionTypeCode = do
      count <- lift (readWord32 byteOrder)
      GeometryCollectionShape <$> replicateM (fromIntegral count) (snd <$> readGeometry)
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

readCoord :: ByteOrder -> Dim -> PtrPeeker.Variable Coord
readCoord byteOrder = \case
  XyDim -> XyCoord <$> readDouble <*> readDouble
  XyzDim -> XyzCoord <$> readDouble <*> readDouble <*> readDouble
  XymDim -> XymCoord <$> readDouble <*> readDouble <*> readDouble
  XyzmDim -> XyzmCoord <$> readDouble <*> readDouble <*> readDouble <*> readDouble
  where
    readDouble = castWord64ToDouble <$> readWord64 byteOrder

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
shapeGen :: Dim -> Int -> QuickCheck.Gen Shape
shapeGen dim size =
  QuickCheck.oneof (if size <= 1 then simple else simple <> compound)
  where
    simple =
      [ PointShape <$> coordGen dim,
        LineStringShape <$> lineStringCoordsGen dim,
        CircularStringShape <$> circularStringCoordsGen dim,
        PolygonShape . pure <$> ringCoordsGen dim,
        MultiPointShape <$> QuickCheck.listOf (coordGen dim)
      ]
    compound =
      [ MultiLineStringShape <$> QuickCheck.listOf (lineStringCoordsGen dim),
        MultiPolygonShape <$> QuickCheck.listOf (QuickCheck.listOf1 (ringCoordsGen dim)),
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
  GeometryCollectionShape shapes ->
    GeometryCollectionShape <$> QuickCheck.shrinkList shrinkShape shapes
  where
    shrinkMembers :: [a] -> [[a]]
    shrinkMembers = QuickCheck.shrinkList (const [])
