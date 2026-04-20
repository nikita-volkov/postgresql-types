module PostgresqlTypes.Geometry
  ( -- * Type
    Geometry (..),
    Shape (..),
    Coord (..),

    -- * Smart constructors
    fromShape,
    fromShapeWithSrid,
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
import Test.QuickCheck (Gen, choose, elements, listOf, listOf1, oneof, resize, sized, vectorOf)
import qualified TextBuilder

-- | PostGIS @geometry@ extension type.
--
-- A PostGIS geometry is a 'Shape' (one of 'Point', 'LineString', 'Polygon',
-- the @Multi@ variants, or a 'GeometryCollection') together with an optional
-- @SRID@ (spatial reference identifier). All coordinates inside a single
-- 'Geometry' value share one dimensionality — every coordinate uses exactly
-- the same combination of optional @z@ and @m@ ordinates. This invariant is
-- enforced by the smart constructor 'fromShape' / 'fromShapeWithSrid'.
--
-- The wire format is PostGIS's
-- [EWKB](https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT),
-- the same binary shape PostGIS emits from @geometry_send@ and accepts in
-- @geometry_recv@. The textual format is the upper-case hex encoding that
-- PostGIS's @geometry_out@ produces and @geometry_in@ accepts.
--
-- The @geometry@ type is not a built-in PostgreSQL type: it is registered at
-- @CREATE EXTENSION postgis@ time and receives a different OID in each
-- database. 'baseOid' and 'arrayOid' are therefore 'Nothing', and drivers
-- resolve the OID by 'typeName' at query time.
-- | __Prefer 'fromShape' / 'fromShapeWithSrid'__: the raw constructor does
-- not enforce dimension consistency across nested shapes.
data Geometry = Geometry
  { -- | Spatial reference ID. Sub-geometries inside a 'GeometryCollection'
    --   inherit this from the outer value; EWKB only stores SRID on the
    --   top-level geometry.
    geometrySrid :: !(Maybe Int32),
    geometryShape :: !Shape
  }
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Geometry)

-- | One of the seven PostGIS/OGC geometry shapes.
data Shape
  = Point !Coord
  | -- | Zero or more coordinates forming a connected line.
    LineString ![Coord]
  | -- | One or more linear rings. The first ring is the exterior, the rest
    --   are interior (holes). Each ring is a closed sequence of coordinates.
    Polygon ![[Coord]]
  | MultiPoint ![Coord]
  | -- | A collection of line strings, each given as its own coordinate list.
    MultiLineString ![[Coord]]
  | -- | A collection of polygons, each given as its list of rings.
    MultiPolygon ![[[Coord]]]
  | -- | Heterogeneous collection of geometries; sub-geometries do not carry
    --   their own SRID and must share the outer dimensionality.
    GeometryCollection ![Shape]
  deriving stock (Eq, Ord, Show, Read)

-- | 2D, 3D (Z), 2D-with-measure (M), or 4D (Z + M) coordinate.
--
-- All coordinates in one 'Geometry' must have matching 'z' and 'm'
-- @'isJust'@/@'isNothing'@ status; the smart constructor checks this.
data Coord = Coord
  { coordX :: !Double,
    coordY :: !Double,
    coordZ :: !(Maybe Double),
    coordM :: !(Maybe Double)
  }
  deriving stock (Eq, Ord, Show, Read)

-- * Smart constructors

-- | Build a 'Geometry' with no SRID after verifying that every coordinate in
--   the shape tree has matching 'coordZ' and 'coordM' dimensionality.
--   Returns a 'Text' error describing the first mismatch if the shape is
--   malformed.
fromShape :: Shape -> Either Text Geometry
fromShape = fromShapeWithSrid Nothing

-- | Build a 'Geometry' with an SRID after the same dimension check as
--   'fromShape'.
fromShapeWithSrid :: Maybe Int32 -> Shape -> Either Text Geometry
fromShapeWithSrid srid shape = do
  void (shapeDim shape)
  pure (Geometry {geometrySrid = srid, geometryShape = shape})

-- * IsScalar instance

instance IsScalar Geometry where
  schemaName = Tagged Nothing
  typeName = Tagged "geometry"
  baseOid = Tagged Nothing
  arrayOid = Tagged Nothing
  typeParams = Tagged []

  binaryEncoder (Geometry srid shape) =
    let dim = either (const XY) id (shapeDimOrXY shape)
     in writeGeometry True srid dim shape

  binaryDecoder = do
    res <- readGeometry True
    pure $ case res of
      Left err ->
        Left
          DecodingError
            { location = ["geometry"],
              reason = ParsingDecodingErrorReason err mempty
            }
      Right (inheritedSrid, shape) -> Right (Geometry inheritedSrid shape)

  textualEncoder (Geometry srid shape) =
    let bytes = Write.toByteString (binaryEncoder (Geometry srid shape))
     in foldMap TextBuilder.hexadecimal (ByteString.unpack bytes)

  textualDecoder = do
    hexText <- Attoparsec.takeText
    case parseHexBytes hexText of
      Left err -> fail ("geometry: " <> err)
      Right bytes -> case PtrPeeker.runVariableOnByteString binaryDecoder bytes of
        Left leftover -> fail ("geometry: binary decoder left " <> show leftover <> " unconsumed bytes")
        Right (Left err) -> fail ("geometry: " <> show err)
        Right (Right value) -> pure value
    where
      parseHexBytes :: Text -> Either String ByteString
      parseHexBytes t = ByteString.pack <$> parseHexPairs (Text.unpack t)
      parseHexPairs :: [Char] -> Either String [Word8]
      parseHexPairs [] = Right []
      parseHexPairs [_] = Left "odd number of hex digits"
      parseHexPairs (a : b : rest) = do
        byte <- hexPairToByte a b
        (byte :) <$> parseHexPairs rest
      hexPairToByte :: Char -> Char -> Either String Word8
      hexPairToByte a b = do
        high <- hexDigitToWord8 a
        low <- hexDigitToWord8 b
        pure (high * 16 + low)
      hexDigitToWord8 :: Char -> Either String Word8
      hexDigitToWord8 c
        | c >= '0' && c <= '9' = Right (fromIntegral (ord c - ord '0'))
        | c >= 'a' && c <= 'f' = Right (fromIntegral (ord c - ord 'a' + 10))
        | c >= 'A' && c <= 'F' = Right (fromIntegral (ord c - ord 'A' + 10))
        | otherwise = Left ("invalid hex digit: " ++ [c])

-- * Arbitrary / Hashable

instance Hashable Geometry where
  -- Hash via the canonical EWKB bytes so two equal 'Geometry' values always
  -- hash the same, independent of any structural irrelevance. The
  -- binaryEncoder is bit-stable.
  hashWithSalt salt geom =
    salt `hashWithSalt` Write.toByteString (binaryEncoder geom)

instance Arbitrary Geometry where
  arbitrary = do
    -- PostGIS treats @SRID = 0@ as "no SRID" and drops the SRID flag on
    -- output, so @SRID 0@ doesn't round-trip exactly. Stick to the
    -- positive half of 'Int32' — which is where real EPSG / spatial_ref_sys
    -- codes live — so wire-format round-trip is an equality.
    srid <- oneof [pure Nothing, Just <$> choose (1, maxBound)]
    dim <- elements [XY, XYZ, XYM, XYZM]
    shape <- sized (shapeGen dim)
    pure (Geometry srid shape)
  shrink (Geometry srid shape) =
    [Geometry srid' shape | srid' <- shrink srid, maybe True (> 0) srid']

-- * Dimension handling

data Dim = XY | XYZ | XYM | XYZM
  deriving stock (Eq, Show)

dimOfCoord :: Coord -> Dim
dimOfCoord (Coord _ _ Nothing Nothing) = XY
dimOfCoord (Coord _ _ (Just _) Nothing) = XYZ
dimOfCoord (Coord _ _ Nothing (Just _)) = XYM
dimOfCoord (Coord _ _ (Just _) (Just _)) = XYZM

-- | Recursively determine the dimension of a shape, failing if the tree
-- contains inconsistent coordinates. Returns 'Nothing' when no coordinates
-- are present (e.g. an empty @LineString@); callers default those to 'XY'.
shapeDim :: Shape -> Either Text (Maybe Dim)
shapeDim = go Nothing
  where
    go :: Maybe Dim -> Shape -> Either Text (Maybe Dim)
    go acc = \case
      Point c -> Just <$> combine acc (dimOfCoord c)
      LineString cs -> foldCoords acc cs
      Polygon rings -> foldM goRing acc rings
      MultiPoint cs -> foldCoords acc cs
      MultiLineString lss -> foldM foldCoords acc lss
      MultiPolygon polys -> foldM (foldM goRing) acc polys
      GeometryCollection shapes -> foldM go acc shapes
    goRing :: Maybe Dim -> [Coord] -> Either Text (Maybe Dim)
    goRing = foldCoords
    foldCoords :: Maybe Dim -> [Coord] -> Either Text (Maybe Dim)
    foldCoords = foldM (\a c -> Just <$> combine a (dimOfCoord c))
    combine :: Maybe Dim -> Dim -> Either Text Dim
    combine Nothing d = Right d
    combine (Just d) d'
      | d == d' = Right d
      | otherwise =
          Left
            ( "geometry: inconsistent coordinate dimensions — got "
                <> Text.pack (show d')
                <> " after "
                <> Text.pack (show d)
            )

shapeDimOrXY :: Shape -> Either Text Dim
shapeDimOrXY = fmap (fromMaybe XY) . shapeDim

-- * Type codes and flag bits

typeCodePoint, typeCodeLineString, typeCodePolygon :: Word32
typeCodeMultiPoint, typeCodeMultiLineString, typeCodeMultiPolygon :: Word32
typeCodeGeometryCollection :: Word32
typeCodePoint = 1
typeCodeLineString = 2
typeCodePolygon = 3
typeCodeMultiPoint = 4
typeCodeMultiLineString = 5
typeCodeMultiPolygon = 6
typeCodeGeometryCollection = 7

flagZ, flagM, flagSRID :: Word32
flagZ = 0x80000000
flagM = 0x40000000
flagSRID = 0x20000000

dimFlags :: Dim -> Word32
dimFlags XY = 0
dimFlags XYZ = flagZ
dimFlags XYM = flagM
dimFlags XYZM = flagZ .|. flagM

typeCodeOfShape :: Shape -> Word32
typeCodeOfShape = \case
  Point {} -> typeCodePoint
  LineString {} -> typeCodeLineString
  Polygon {} -> typeCodePolygon
  MultiPoint {} -> typeCodeMultiPoint
  MultiLineString {} -> typeCodeMultiLineString
  MultiPolygon {} -> typeCodeMultiPolygon
  GeometryCollection {} -> typeCodeGeometryCollection

-- * Binary encoder

-- | Encode a geometry as EWKB. The @topLevel@ flag controls whether the SRID
--   flag and value are emitted — sub-geometries within Multi* / Collection
--   values inherit the outer SRID and do not repeat it.
writeGeometry :: Bool -> Maybe Int32 -> Dim -> Shape -> Write.Write
writeGeometry topLevel srid dim shape =
  let sridFlag = if topLevel && isJust srid then flagSRID else 0
      header = lWord32 (typeCodeOfShape shape .|. dimFlags dim .|. sridFlag)
      sridField = case (topLevel, srid) of
        (True, Just s) -> lWord32 (fromIntegral s)
        _ -> mempty
   in Write.word8 0x01 -- little-endian / NDR byte order
        <> header
        <> sridField
        <> writePayload dim shape

-- | Little-endian Word32 encode helper (alias to keep the caller readable).
lWord32 :: Word32 -> Write.Write
lWord32 = Write.lWord32

lWord64 :: Word64 -> Write.Write
lWord64 = Write.lWord64

writePayload :: Dim -> Shape -> Write.Write
writePayload dim = \case
  Point c -> writeCoord dim c
  LineString cs -> lWord32 (fromIntegral (length cs)) <> foldMap (writeCoord dim) cs
  Polygon rings -> lWord32 (fromIntegral (length rings)) <> foldMap (writeRing dim) rings
  MultiPoint cs ->
    lWord32 (fromIntegral (length cs))
      <> foldMap (writeGeometry False Nothing dim . Point) cs
  MultiLineString lss ->
    lWord32 (fromIntegral (length lss))
      <> foldMap (writeGeometry False Nothing dim . LineString) lss
  MultiPolygon polys ->
    lWord32 (fromIntegral (length polys))
      <> foldMap (writeGeometry False Nothing dim . Polygon) polys
  GeometryCollection shapes ->
    lWord32 (fromIntegral (length shapes))
      <> foldMap (writeGeometry False Nothing dim) shapes

writeRing :: Dim -> [Coord] -> Write.Write
writeRing dim cs = lWord32 (fromIntegral (length cs)) <> foldMap (writeCoord dim) cs

writeCoord :: Dim -> Coord -> Write.Write
writeCoord dim (Coord x y mz mm) =
  encodeDouble x
    <> encodeDouble y
    <> (if includesZ dim then encodeDouble (fromMaybe 0 mz) else mempty)
    <> (if includesM dim then encodeDouble (fromMaybe 0 mm) else mempty)
  where
    encodeDouble = lWord64 . castDoubleToWord64

includesZ :: Dim -> Bool
includesZ XYZ = True
includesZ XYZM = True
includesZ _ = False

includesM :: Dim -> Bool
includesM XYM = True
includesM XYZM = True
includesM _ = False

-- * Binary decoder

-- | Decoder result: inherited SRID (top-level only) plus the decoded shape.
--   Sub-geometries do not produce an SRID; their value is 'Nothing'.
readGeometry :: Bool -> PtrPeeker.Variable (Either Text (Maybe Int32, Shape))
readGeometry topLevel = do
  byteOrderFlag <- PtrPeeker.fixed PtrPeeker.unsignedInt1
  let littleEndian = byteOrderFlag /= 0
  typeWithFlags <- readWord32 littleEndian
  let hasZ = testBit typeWithFlags 31
      hasM = testBit typeWithFlags 30
      hasSRID = testBit typeWithFlags 29
      rawType = typeWithFlags .&. 0x1FFFFFFF
      dim = case (hasZ, hasM) of
        (False, False) -> XY
        (True, False) -> XYZ
        (False, True) -> XYM
        (True, True) -> XYZM
  -- EWKB allows sub-geometries to carry their own SRID field, but PostGIS
  -- ignores them and inherits from the outer geometry. We consume the bytes
  -- to keep the stream aligned, then discard the value when we're not at
  -- the top level.
  srid <-
    if hasSRID
      then Just . (fromIntegral :: Word32 -> Int32) <$> readWord32 littleEndian
      else pure Nothing
  result <- decodeShape littleEndian dim rawType
  pure $ case result of
    Left err -> Left err
    Right shape -> Right (if topLevel then srid else Nothing, shape)

decodeShape :: Bool -> Dim -> Word32 -> PtrPeeker.Variable (Either Text Shape)
decodeShape le dim typeCode
  | typeCode == typeCodePoint = do
      c <- readCoord le dim
      pure (Right (Point c))
  | typeCode == typeCodeLineString = do
      n <- readWord32 le
      cs <- replicateM (fromIntegral n) (readCoord le dim)
      pure (Right (LineString cs))
  | typeCode == typeCodePolygon = do
      nRings <- readWord32 le
      rings <- replicateM (fromIntegral nRings) $ do
        np <- readWord32 le
        replicateM (fromIntegral np) (readCoord le dim)
      pure (Right (Polygon rings))
  | typeCode == typeCodeMultiPoint = decodeMulti le (\case Point c -> Just c; _ -> Nothing) MultiPoint
  | typeCode == typeCodeMultiLineString = decodeMulti le (\case LineString cs -> Just cs; _ -> Nothing) MultiLineString
  | typeCode == typeCodeMultiPolygon = decodeMulti le (\case Polygon rings -> Just rings; _ -> Nothing) MultiPolygon
  | typeCode == typeCodeGeometryCollection = do
      n <- readWord32 le
      subs <- replicateM (fromIntegral n) (readGeometry False)
      pure $ case sequence subs of
        Left err -> Left err
        Right pairs -> Right (GeometryCollection (map snd pairs))
  | otherwise = pure (Left ("geometry: unsupported type code " <> Text.pack (show typeCode)))

decodeMulti ::
  Bool ->
  (Shape -> Maybe a) ->
  ([a] -> Shape) ->
  PtrPeeker.Variable (Either Text Shape)
decodeMulti le project ctor = do
  n <- readWord32 le
  subs <- replicateM (fromIntegral n) (readGeometry False)
  pure $ case sequence subs of
    Left err -> Left err
    Right pairs ->
      case traverse (project . snd) pairs of
        Just xs -> Right (ctor xs)
        Nothing -> Left "geometry: MultiXxx contained a sub-geometry of the wrong shape"

readWord32 :: Bool -> PtrPeeker.Variable Word32
readWord32 le = PtrPeeker.fixed (if le then PtrPeeker.leUnsignedInt4 else PtrPeeker.beUnsignedInt4)

readWord64 :: Bool -> PtrPeeker.Variable Word64
readWord64 le = PtrPeeker.fixed (if le then PtrPeeker.leUnsignedInt8 else PtrPeeker.beUnsignedInt8)

readDouble :: Bool -> PtrPeeker.Variable Double
readDouble le = castWord64ToDouble <$> readWord64 le

readCoord :: Bool -> Dim -> PtrPeeker.Variable Coord
readCoord le dim = do
  x <- readDouble le
  y <- readDouble le
  (z, m) <- case dim of
    XY -> pure (Nothing, Nothing)
    XYZ -> do z <- readDouble le; pure (Just z, Nothing)
    XYM -> do m <- readDouble le; pure (Nothing, Just m)
    XYZM -> do z <- readDouble le; m <- readDouble le; pure (Just z, Just m)
  pure (Coord x y z m)

-- * QuickCheck generators

-- | Generates EWKB-valid 'Shape' values:
--
-- * 'LineString' has at least 2 coordinates.
-- * 'Polygon' rings are closed (@first == last@) with at least 4 coordinates.
-- * 'MultiLineString' / 'MultiPolygon' sub-items respect the same rules.
shapeGen :: Dim -> Int -> Gen Shape
shapeGen dim n
  | n <= 1 =
      oneof
        [ Point <$> coordGen dim,
          LineString <$> lineStringCoords dim,
          singleRingPolygonGen dim,
          MultiPoint <$> listOf (coordGen dim)
        ]
  | otherwise =
      oneof
        [ Point <$> coordGen dim,
          LineString <$> lineStringCoords dim,
          singleRingPolygonGen dim,
          MultiPoint <$> listOf (coordGen dim),
          MultiLineString <$> listOf (lineStringCoords dim),
          MultiPolygon <$> listOf (listOf1 (polygonRingCoords dim)),
          GeometryCollection <$> resize (n `div` 4) (listOf (shapeGen dim (n `div` 4)))
        ]

lineStringCoords :: Dim -> Gen [Coord]
lineStringCoords dim = do
  -- OGC requires LineString to have at least 2 coordinates.
  extra <- choose (0, 6 :: Int)
  vectorOf (2 + extra) (coordGen dim)

polygonRingCoords :: Dim -> Gen [Coord]
polygonRingCoords dim = do
  -- A ring is closed (first == last) and has at least 4 coordinates
  -- (3 distinct + the closing repeat of the first).
  extra <- choose (0, 5 :: Int)
  interior <- vectorOf (3 + extra) (coordGen dim)
  case interior of
    (first : _) -> pure (interior ++ [first])
    [] -> error "polygonRingCoords: impossible — vectorOf with positive length returned []"

singleRingPolygonGen :: Dim -> Gen Shape
singleRingPolygonGen dim = Polygon . (: []) <$> polygonRingCoords dim

coordGen :: Dim -> Gen Coord
coordGen dim = do
  x <- arbitrary
  y <- arbitrary
  (z, m) <- case dim of
    XY -> pure (Nothing, Nothing)
    XYZ -> (,) <$> (Just <$> arbitrary) <*> pure Nothing
    XYM -> (,) Nothing . Just <$> arbitrary
    XYZM -> (,) <$> (Just <$> arbitrary) <*> (Just <$> arbitrary)
  pure (Coord x y z m)
