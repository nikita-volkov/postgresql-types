module NumericSpec (spec) where

import Data.Proxy (Proxy (..))
import qualified Data.Scientific as Scientific
import Data.Typeable (Typeable, typeRep)
import qualified GHC.TypeLits as TypeLits
import qualified PostgresqlTypes.Types.Numeric as Numeric
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import Prelude

spec :: Spec
spec = do
  describe "roundtrips" do
    roundtripSuite (Proxy @0) (Proxy @0)
    roundtripSuite (Proxy @10) (Proxy @4)
    roundtripSuite (Proxy @5) (Proxy @2)

  describe "clamping and validation" do
    it "clamps to scale and precision for Numeric(5,2)" do
      let input = Scientific.scientific 999999 (-3) -- 999.999
          normalized = Numeric.normalizeFromScientific @5 @2 input
      Numeric.normalizeToScientific normalized `shouldBe` Scientific.scientific 99999 (-2)

    it "rejects values exceeding precision for Numeric(5,2)" do
      let invalid = Scientific.scientific 1234567 (-2) -- 12345.67 -> 7 total digits > precision 5
      Numeric.projectFromScientific @5 @2 invalid `shouldBe` Nothing

    it "accepts valid values for Numeric(5,2)" do
      let valid = Scientific.scientific 12345 (-2) -- 123.45 fits precision and scale
      Numeric.projectFromScientific @5 @2 valid `shouldBe` Just (Numeric.normalizeFromScientific valid)

    it "rejects scientifics outside PostgreSQL limits for Numeric(0,0)" do
      let tooManyIntegerDigits = Scientific.scientific 1 131074 -- digits before decimal exceed 131072
          tooManyFractionDigits = Scientific.scientific 1 (-20000) -- digits after decimal exceed 16383
      Numeric.projectFromScientific @0 @0 tooManyIntegerDigits `shouldBe` Nothing
      Numeric.projectFromScientific @0 @0 tooManyFractionDigits `shouldBe` Nothing

  describe "special values" do
    it "normalizeToScientific renders special values as 0" do
      Numeric.normalizeToScientific (Numeric.nan :: Numeric.Numeric 0 0) `shouldBe` 0
      Numeric.normalizeToScientific (Numeric.posInfinity :: Numeric.Numeric 0 0) `shouldBe` 0
      Numeric.normalizeToScientific (Numeric.negInfinity :: Numeric.Numeric 0 0) `shouldBe` 0

    it "predicates detect special values" do
      Numeric.isNaN (Numeric.nan :: Numeric.Numeric 0 0) `shouldBe` True
      Numeric.isPosInfinity (Numeric.posInfinity :: Numeric.Numeric 0 0) `shouldBe` True
      Numeric.isNegInfinity (Numeric.negInfinity :: Numeric.Numeric 0 0) `shouldBe` True
      Numeric.isNaN (Numeric.normalizeFromScientific 0 :: Numeric.Numeric 0 0) `shouldBe` False
      Numeric.isPosInfinity (Numeric.normalizeFromScientific 0 :: Numeric.Numeric 0 0) `shouldBe` False
      Numeric.isNegInfinity (Numeric.normalizeFromScientific 0 :: Numeric.Numeric 0 0) `shouldBe` False

    it "projectToScientific rejects special values" do
      Numeric.projectToScientific (Numeric.nan :: Numeric.Numeric 0 0) `shouldBe` Nothing
      Numeric.projectToScientific (Numeric.posInfinity :: Numeric.Numeric 0 0) `shouldBe` Nothing
      Numeric.projectToScientific (Numeric.negInfinity :: Numeric.Numeric 0 0) `shouldBe` Nothing

-- | Property suites shared across several numeric precisions/scales.
roundtripSuite ::
  forall precision scale.
  ( TypeLits.KnownNat precision,
    TypeLits.KnownNat scale,
    QuickCheck.Arbitrary (Numeric.Numeric precision scale),
    Show (Numeric.Numeric precision scale),
    Eq (Numeric.Numeric precision scale),
    Typeable (Numeric.Numeric precision scale)
  ) =>
  Proxy precision ->
  Proxy scale ->
  Spec
roundtripSuite _ _ =
  describe (show (typeRep (Proxy @(Numeric.Numeric precision scale)))) do
    it "project -> normalize restores finite values" do
      QuickCheck.property \(value :: Numeric.Numeric precision scale) ->
        case Numeric.projectToScientific value of
          Nothing -> QuickCheck.property True
          Just sci -> Numeric.normalizeFromScientific @precision @scale sci === value

    it "projectFromScientific . projectToScientific is identity on finite values" do
      QuickCheck.property \(value :: Numeric.Numeric precision scale) ->
        case Numeric.projectToScientific value of
          Nothing -> QuickCheck.property True
          Just sci -> Numeric.projectFromScientific @precision @scale sci === Just value

    it "normalizeFromScientific produces projectable values" do
      QuickCheck.property \(sci :: Scientific.Scientific) ->
        let value = Numeric.normalizeFromScientific @precision @scale sci
            normalizedSci = Numeric.normalizeToScientific value
         in Numeric.projectFromScientific @precision @scale normalizedSci === Just value
