module PostgresqlTypes.GeometrySpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Either (isLeft)
import qualified PostgresqlTypes.Geometry as Geometry
import PostgresqlTypes.Geometry (Coord (Coord), Geometry (Geometry), Shape (..))
import Test.Hspec
import Test.QuickCheck
import Data.String (fromString)
import Test.QuickCheck.Instances ()
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Geometry)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Geometry)

  describe "Smart constructors" do
    describe "fromShape" do
      it "accepts an XY Point" do
        Geometry.fromShape (Point (Coord 1 2 Nothing Nothing))
          `shouldBe` Right (Geometry Nothing (Point (Coord 1 2 Nothing Nothing)))

      it "accepts a consistently-dimensioned LineString" do
        let cs = [Coord 0 0 (Just 1) Nothing, Coord 1 1 (Just 2) Nothing]
        Geometry.fromShape (LineString cs)
          `shouldBe` Right (Geometry Nothing (LineString cs))

      it "rejects a LineString mixing XY and XYZ coordinates" do
        let cs = [Coord 0 0 Nothing Nothing, Coord 1 1 (Just 2) Nothing]
        Geometry.fromShape (LineString cs) `shouldSatisfy` isLeft

      it "rejects a GeometryCollection mixing XY and XYM shapes" do
        let mixed =
              GeometryCollection
                [ Point (Coord 0 0 Nothing Nothing),
                  Point (Coord 1 1 Nothing (Just 3))
                ]
        Geometry.fromShape mixed `shouldSatisfy` isLeft

    describe "fromShapeWithSrid" do
      it "threads the SRID through to the Geometry value" do
        let srid = Just 4326
            shape = Point (Coord 13.4 52.5 Nothing Nothing)
        Geometry.fromShapeWithSrid srid shape
          `shouldBe` Right (Geometry srid shape)

  describe "Accessors" do
    it "geometrySrid extracts the SRID" do
      Geometry.geometrySrid (Geometry (Just 4326) (Point (Coord 0 0 Nothing Nothing)))
        `shouldBe` Just 4326

    it "geometryShape extracts the Shape" do
      let shape = Point (Coord 1 2 Nothing Nothing)
      Geometry.geometryShape (Geometry Nothing shape) `shouldBe` shape

  describe "Binary wire format" do
    it "decodes a canonical PostGIS EWKB Point(1,2) with SRID 4326" do
      -- Hex from PostGIS: SELECT ST_AsEWKB(ST_SetSRID(ST_MakePoint(1, 2), 4326));
      -- byte-order NDR (01) + type 1 with SRID flag (01000020) + SRID 4326 (E6100000)
      --   + x=1.0 (000000000000F03F) + y=2.0 (0000000000000040)
      let hex = "0101000020E6100000000000000000F03F0000000000000040"
      read (show hex) `shouldBe` hex -- placate the compiler (test keeps hex in scope)
      let g = (fromString hex :: Geometry)
      g `shouldBe` Geometry (Just 4326) (Point (Coord 1 2 Nothing Nothing))

    it "round-trips a Point through hex EWKB" do
      let g = Geometry Nothing (Point (Coord 3.5 (-1.25) Nothing Nothing))
      read (show g) `shouldBe` g

    it "round-trips an XYZM LineString through hex EWKB" do
      let cs = [Coord 0 0 (Just 1) (Just 2), Coord 3 4 (Just 5) (Just 6)]
          g = Geometry (Just 4326) (LineString cs)
      read (show g) `shouldBe` g

  describe "Property: full binary roundtrip for arbitrary geometries" do
    it "holds for arbitrary shapes and SRIDs" $
      property \(g :: Geometry) ->
        read (show g) === g
