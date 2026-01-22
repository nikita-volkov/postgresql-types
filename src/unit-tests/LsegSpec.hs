module LsegSpec (spec) where

import qualified PostgresqlTypes.Lseg as Lseg
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Lseg" do
    describe "Constructors" do
      describe "fromEndpoints" do
        it "creates Lseg from two points (4-tuple)" do
          let lseg = Lseg.fromEndpoints (0.0, 0.0, 3.0, 4.0)
              (x1, y1, x2, y2) = Lseg.toEndpoints lseg
          (x1, y1, x2, y2) `shouldBe` (0.0, 0.0, 3.0, 4.0)

        it "creates Lseg with coincident endpoints" do
          let lseg = Lseg.fromEndpoints (1.0, 1.0, 1.0, 1.0)
              (x1, y1, x2, y2) = Lseg.toEndpoints lseg
          (x1, y1, x2, y2) `shouldBe` (1.0, 1.0, 1.0, 1.0)

    describe "Accessors" do
      describe "toEndpoints" do
        it "extracts endpoints as 4-tuple" do
          let lseg = Lseg.fromEndpoints (1.5, 2.5, 3.5, 4.5)
              endpoints = Lseg.toEndpoints lseg
          endpoints `shouldBe` (1.5, 2.5, 3.5, 4.5)

    describe "Property Tests" do
      it "roundtrips through toEndpoints and fromEndpoints" do
        property \(lseg :: Lseg.Lseg) ->
          let endpoints = Lseg.toEndpoints lseg
              lseg' = Lseg.fromEndpoints endpoints
           in lseg' === lseg
