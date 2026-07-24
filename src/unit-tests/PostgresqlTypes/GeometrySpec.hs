module PostgresqlTypes.GeometrySpec (spec) where

import qualified Data.Attoparsec.Text
import Data.Data (Proxy (Proxy))
import Data.Either
import Data.Hashable (hashWithSalt)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified PostgresqlTypes.Algebra
import PostgresqlTypes.Geometry (Coord (..), Geometry, Shape (..))
import qualified PostgresqlTypes.Geometry as Geometry
import Test.Hspec
import Test.QuickCheck
import qualified TextBuilder
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Geometry)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Geometry)

  describe "Constructors" do
    describe "refineFromShape" do
      it "accepts a 2D point" do
        let shape = PointShape (XyCoord 1 2)
        fmap Geometry.toShape (Geometry.refineFromShape shape) `shouldBe` Just shape

      it "accepts a line string of uniformly 3D coordinates" do
        let shape = LineStringShape [XyzCoord 0 0 1, XyzCoord 1 1 2]
        fmap Geometry.toShape (Geometry.refineFromShape shape) `shouldBe` Just shape

      it "rejects a line string mixing 2D and 3D coordinates" do
        Geometry.refineFromShape (LineStringShape [XyCoord 0 0, XyzCoord 1 1 2])
          `shouldBe` Nothing

      it "rejects a collection mixing plain and measured shapes" do
        Geometry.refineFromShape
          ( GeometryCollectionShape
              [ PointShape (XyCoord 0 0),
                PointShape (XymCoord 1 1 3)
              ]
          )
          `shouldBe` Nothing

    describe "refineFromShapeAndSrid" do
      it "threads the SRID through to the geometry" do
        let shape = PointShape (XyCoord 13.4 52.5)
        fmap Geometry.toSrid (Geometry.refineFromShapeAndSrid shape (Just 4326))
          `shouldBe` Just (Just 4326)

      it "rejects SRID 0" do
        Geometry.refineFromShapeAndSrid (PointShape (XyCoord 0 0)) (Just 0)
          `shouldBe` Nothing

      it "rejects a negative SRID" do
        Geometry.refineFromShapeAndSrid (PointShape (XyCoord 0 0)) (Just (-5))
          `shouldBe` Nothing

      it "accepts Nothing" do
        Geometry.refineFromShapeAndSrid (PointShape (XyCoord 0 0)) Nothing
          `shouldSatisfy` isJust

      it "accepts a positive SRID" do
        Geometry.refineFromShapeAndSrid (PointShape (XyCoord 0 0)) (Just 1)
          `shouldSatisfy` isJust

  describe "Accessors" do
    describe "toSrid" do
      it "extracts the SRID" do
        fmap Geometry.toSrid (Geometry.refineFromShapeAndSrid (PointShape (XyCoord 0 0)) (Just 4326))
          `shouldBe` Just (Just 4326)

    describe "toShape" do
      it "extracts the shape" do
        let shape = PointShape (XyCoord 1 2)
        fmap Geometry.toShape (Geometry.refineFromShape shape) `shouldBe` Just shape

  describe "Wire format" do
    -- Fixture produced by PostGIS itself:
    -- SELECT ST_AsEWKB(ST_SetSRID(ST_MakePoint(1, 2), 4326));
    --
    -- Byte-order marker NDR (01), type 1 with the SRID flag (01000020), SRID 4326 (E6100000),
    -- x = 1.0 (000000000000F03F), y = 2.0 (0000000000000040).
    let pointHex = "0101000020E6100000000000000000F03F0000000000000040"
        point = Geometry.refineFromShapeAndSrid (PointShape (XyCoord 1 2)) (Just 4326)

    it "decodes a point PostGIS produced" do
      fmap Just (decodeHex pointHex) `shouldBe` Right point

    it "encodes a point the way PostGIS does" do
      fmap encodeHex point `shouldBe` Just (Text.toLower pointHex)

    it "accepts big-endian input" do
      -- The same point, XDR: marker 00, then every multi-byte field byte-swapped.
      fmap Just (decodeHex "0020000001000010E63FF00000000000004000000000000000")
        `shouldBe` Right point

    it "rejects an invalid byte-order marker" do
      decodeHex "0201000000000000000000F03F0000000000000040" `shouldSatisfy` isLeft

    it "rejects an unknown type code" do
      decodeHex "0163000000000000000000F03F0000000000000040" `shouldSatisfy` isLeft

    it "rejects trailing bytes" do
      decodeHex "0101000000000000000000F03F00000000000000400000" `shouldSatisfy` isLeft

    it "rejects a truncated payload" do
      decodeHex "0101000000000000000000F03F" `shouldSatisfy` isLeft

    -- Fixture derived by hand from the EWKB layout (no live PostGIS instance required for this test):
    -- a CircularString of one arc through (0,0), (1,2), (2,0), with SRID 4326.
    --
    -- Byte-order marker NDR (01), type 8 (CircularString) with the SRID flag (08000020), SRID 4326
    -- (E6100000), coordinate count 3 (03000000), then the three coordinates: (0,0), (1,2), (2,0),
    -- each ordinate as a little-endian IEEE-754 double.
    let circularStringHex = "0108000020E61000000300000000000000000000000000000000000000000000000000F03F000000000000004000000000000000400000000000000000"
        circularString =
          Geometry.refineFromShapeAndSrid
            (CircularStringShape [XyCoord 0 0, XyCoord 1 2, XyCoord 2 0])
            (Just 4326)

    it "decodes a circular string" do
      fmap Just (decodeHex circularStringHex) `shouldBe` Right circularString

    it "encodes a circular string the way PostGIS does" do
      fmap encodeHex circularString `shouldBe` Just (Text.toLower circularStringHex)

    it "rejects a multi-point whose member is a line string" do
      decodeHex
        ( mconcat
            [ "01", -- NDR
              "04000000", -- MultiPoint
              "01000000", -- 1 member
              "01", -- NDR
              "02000000", -- LineString, where a Point is required
              "02000000", -- 2 coordinates
              "000000000000F03F0000000000000040",
              "00000000000008400000000000001040"
            ]
        )
        `shouldSatisfy` isLeft

    it "rejects a collection whose members disagree on dimensionality" do
      decodeHex
        ( mconcat
            [ "01", -- NDR
              "07000000", -- GeometryCollection
              "02000000", -- 2 members
              "01", -- NDR
              "01000000", -- Point, 2D
              "000000000000F03F0000000000000040",
              "01", -- NDR
              "01000080", -- Point, 3D
              "000000000000F03F00000000000000400000000000000840"
            ]
        )
        `shouldSatisfy` isLeft

    -- A triangle is a polygon restricted to 0 or 1 rings: a ring-count word32, followed by that one
    -- ring's coordinates when present.
    let emptyTriangleHex =
          mconcat
            [ "01", -- NDR
              "11000000", -- type 17 (Triangle), no flags
              "00000000" -- 0 rings
            ]
        emptyTriangle = Geometry.refineFromShape (TriangleShape [])

    it "decodes an empty triangle" do
      fmap Just (decodeHex emptyTriangleHex) `shouldBe` Right emptyTriangle

    it "encodes an empty triangle" do
      fmap encodeHex emptyTriangle `shouldBe` Just (Text.toLower emptyTriangleHex)

    let nonEmptyTriangleHex =
          mconcat
            [ "01", -- NDR
              "11000000", -- type 17 (Triangle), no flags
              "01000000", -- 1 ring
              "04000000", -- 4 coordinates
              "00000000000000000000000000000000", -- (0, 0)
              "00000000000010400000000000000000", -- (4, 0)
              "00000000000000000000000000000840", -- (0, 3)
              "00000000000000000000000000000000" -- (0, 0), closing the ring
            ]
        nonEmptyTriangle =
          Geometry.refineFromShape
            (TriangleShape [XyCoord 0 0, XyCoord 4 0, XyCoord 0 3, XyCoord 0 0])

    it "decodes a non-empty triangle" do
      fmap Just (decodeHex nonEmptyTriangleHex) `shouldBe` Right nonEmptyTriangle

    it "encodes a non-empty triangle" do
      fmap encodeHex nonEmptyTriangle `shouldBe` Just (Text.toLower nonEmptyTriangleHex)

    it "rejects a triangle with more than one ring" do
      decodeHex
        ( mconcat
            [ "01", -- NDR
              "11000000", -- Triangle
              "02000000" -- 2 rings, which a Triangle disallows
            ]
        )
        `shouldSatisfy` isLeft

  describe "Hashable" do
    it "agrees with Eq on negative zero" do
      -- Hashing the EWKB bytes would break this: the two coordinates are equal, yet their IEEE
      -- representations differ.
      let positive = Geometry.refineFromShape (PointShape (XyCoord 0 0))
          negative = Geometry.refineFromShape (PointShape (XyCoord (-0.0) (-0.0)))
      positive `shouldBe` negative
      fmap (hashWithSalt 0) positive `shouldBe` fmap (hashWithSalt 0) negative

  describe "Property Tests" do
    it "roundtrips through the accessors and refineFromShapeAndSrid" do
      property \(geometry :: Geometry) ->
        Geometry.refineFromShapeAndSrid (Geometry.toShape geometry) (Geometry.toSrid geometry)
          === Just geometry

decodeHex :: Text -> Either String Geometry
decodeHex =
  Data.Attoparsec.Text.parseOnly
    (PostgresqlTypes.Algebra.textualDecoder @Geometry <* Data.Attoparsec.Text.endOfInput)

encodeHex :: Geometry -> Text
encodeHex = TextBuilder.toText . PostgresqlTypes.Algebra.textualEncoder
