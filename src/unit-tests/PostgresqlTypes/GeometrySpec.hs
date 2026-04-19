module PostgresqlTypes.GeometrySpec (spec) where

import qualified Data.ByteString as ByteString
import Data.Data (Proxy (Proxy))
import qualified PostgresqlTypes.Geometry as Geometry
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Geometry.Geometry)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Geometry.Geometry)

  describe "Constructors" do
    describe "fromEWKB" do
      it "wraps a ByteString of EWKB bytes" do
        let bs = ByteString.pack [0x01, 0x01, 0x00, 0x00, 0x00]
            geom = Geometry.fromEWKB bs
        Geometry.toEWKB geom `shouldBe` bs

      it "accepts the empty ByteString" do
        Geometry.toEWKB (Geometry.fromEWKB ByteString.empty) `shouldBe` ByteString.empty

      it "preserves binary data without mangling it" do
        let bs = ByteString.pack [0, 255, 128, 1, 127]
        Geometry.toEWKB (Geometry.fromEWKB bs) `shouldBe` bs

  describe "Accessors" do
    describe "toEWKB" do
      it "extracts the underlying ByteString" do
        let bs = ByteString.pack [10, 20, 30]
        Geometry.toEWKB (Geometry.fromEWKB bs) `shouldBe` bs

  describe "Property Tests" do
    it "roundtrips through toEWKB and fromEWKB" do
      property \(bs :: ByteString.ByteString) ->
        Geometry.toEWKB (Geometry.fromEWKB bs) === bs

    it "roundtrips through fromEWKB and toEWKB" do
      property \(geom :: Geometry.Geometry) ->
        Geometry.fromEWKB (Geometry.toEWKB geom) === geom
