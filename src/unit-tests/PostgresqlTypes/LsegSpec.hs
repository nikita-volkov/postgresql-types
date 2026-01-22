module PostgresqlTypes.LsegSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified PostgresqlTypes.Lseg as Lseg
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Lseg.Lseg)

  describe "Constructors" do
    describe "fromEndpoints" do
      it "creates Lseg from two points (4 params)" do
        let lseg = Lseg.fromEndpoints 0.0 0.0 3.0 4.0
        Lseg.toX1 lseg `shouldBe` 0.0
        Lseg.toY1 lseg `shouldBe` 0.0
        Lseg.toX2 lseg `shouldBe` 3.0
        Lseg.toY2 lseg `shouldBe` 4.0

      it "creates Lseg with coincident endpoints" do
        let lseg = Lseg.fromEndpoints 1.0 1.0 1.0 1.0
        (Lseg.toX1 lseg, Lseg.toY1 lseg, Lseg.toX2 lseg, Lseg.toY2 lseg) `shouldBe` (1.0, 1.0, 1.0, 1.0)

  describe "Accessors" do
    it "extracts individual coordinates" do
      let lseg = Lseg.fromEndpoints 1.5 2.5 3.5 4.5
      Lseg.toX1 lseg `shouldBe` 1.5
      Lseg.toY1 lseg `shouldBe` 2.5
      Lseg.toX2 lseg `shouldBe` 3.5
      Lseg.toY2 lseg `shouldBe` 4.5

  describe "Property Tests" do
    it "roundtrips through accessors and fromEndpoints" do
      property \(lseg :: Lseg.Lseg) ->
        let lseg' = Lseg.fromEndpoints (Lseg.toX1 lseg) (Lseg.toY1 lseg) (Lseg.toX2 lseg) (Lseg.toY2 lseg)
         in lseg' === lseg
