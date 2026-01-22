module PostgresqlTypes.NumericSpec (spec) where

import Control.Monad
import Data.Proxy (Proxy (..))
import qualified Data.Scientific as Scientific
import Data.Typeable (Typeable, typeRep)
import qualified GHC.TypeLits as TypeLits
import qualified PostgresqlTypes.Numeric as Numeric
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @(Numeric.Numeric 0 0))

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @(Numeric.Numeric 0 0))

  describe "By precision and scale" do
    byPrecisionAndScale (Proxy @0) (Proxy @0)
    byPrecisionAndScale (Proxy @10) (Proxy @4)
    byPrecisionAndScale (Proxy @5) (Proxy @2)
    byPrecisionAndScale (Proxy @5) (Proxy @0)

  describe "clamping and validation" do
    it "clamps to scale and precision for Numeric(5,2)" do
      let input = Scientific.scientific 999999 (-3) -- 999.999
          normalized = Numeric.normalizeFromScientific @5 @2 input
      Numeric.normalizeToScientific normalized `shouldBe` Scientific.scientific 99999 (-2)

    it "rejects values exceeding precision for Numeric(5,2)" do
      let invalid = Scientific.scientific 1234567 (-2) -- 12345.67 -> 7 total digits > precision 5
      Numeric.refineFromScientific @5 @2 invalid `shouldBe` Nothing

    it "accepts valid values for Numeric(5,2)" do
      let valid = Scientific.scientific 12345 (-2) -- 123.45 fits precision and scale
      Numeric.refineFromScientific @5 @2 valid `shouldBe` Just (Numeric.normalizeFromScientific valid)

    it "rejects scientifics outside PostgreSQL limits for Numeric(0,0)" do
      let tooManyIntegerDigits = Scientific.scientific 1 131074 -- digits before decimal exceed 131072
          tooManyFractionDigits = Scientific.scientific 1 (-20000) -- digits after decimal exceed 16383
      Numeric.refineFromScientific @0 @0 tooManyIntegerDigits `shouldBe` Nothing
      Numeric.refineFromScientific @0 @0 tooManyFractionDigits `shouldBe` Nothing

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

    it "refineToScientific rejects special values" do
      Numeric.refineToScientific (Numeric.nan :: Numeric.Numeric 0 0) `shouldBe` Nothing
      Numeric.refineToScientific (Numeric.posInfinity :: Numeric.Numeric 0 0) `shouldBe` Nothing
      Numeric.refineToScientific (Numeric.negInfinity :: Numeric.Numeric 0 0) `shouldBe` Nothing

  describe "Invalid type params" do
    describe "refineFromScientific" do
      it "Fails on any scientific" do
        QuickCheck.property \(sci :: Scientific.Scientific) ->
          Numeric.refineFromScientific @2 @5 sci === Nothing

    describe "normalizeFromScientific" do
      it "Always returns NaN" do
        QuickCheck.property \(sci :: Scientific.Scientific) ->
          let normalized = Numeric.normalizeFromScientific @2 @5 sci
           in Numeric.isNaN normalized

  describe "Valid type params" do
    describe "refineFromScientific" do
      it "Rejects values exceeding scale" do
        let sci = Scientific.scientific 12345 (-3) -- 12.345 has scale 3 > 2
        Numeric.refineFromScientific @5 @2 sci `shouldBe` Nothing

      it "Rejects values exceeding precision" do
        let sci = Scientific.scientific 123456 (-2) -- 12345.6 has precision 6 > 5
        Numeric.refineFromScientific @5 @2 sci `shouldBe` Nothing

      it "Accepts values within precision and scale" do
        let sci = Scientific.scientific 12345 (-2) -- 123.45 fits precision and scale
        Numeric.refineFromScientific @5 @2 sci `shouldBe` Just (Numeric.normalizeFromScientific sci)

    describe "normalizeFromScientific" do
      it "Clamps scale to 2" do
        let sci = Scientific.scientific 12345 (-4) -- 1.2345 has scale 4 > 2
            normalized = Numeric.normalizeFromScientific @5 @2 sci
        Numeric.normalizeToScientific normalized `shouldBe` Scientific.scientific 123 (-2)

      it "Clamps when input is larger than max" do
        let sci = Scientific.scientific 1234567 (-2) -- 12345.67 has precision 7 > 5
            normalized = Numeric.normalizeFromScientific @5 @2 sci
        Numeric.normalizeToScientific normalized `shouldBe` read "999.99"

      it "Clamps when input is smaller than min" do
        let sci = read "-10000"
            normalized = Numeric.normalizeFromScientific @5 @2 sci
        Numeric.normalizeToScientific normalized `shouldBe` read "-999.99"

-- | Property suites shared across several numeric precisions/scales.
byPrecisionAndScale ::
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
byPrecisionAndScale _ _ = do
  let precision = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
      scale = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int

  describe (show (typeRep (Proxy @(Numeric.Numeric precision scale)))) do
    it "project -> normalize restores finite values" do
      QuickCheck.property \(value :: Numeric.Numeric precision scale) ->
        case Numeric.refineToScientific value of
          Nothing -> QuickCheck.property True
          Just sci -> Numeric.normalizeFromScientific @precision @scale sci === value

    it "refineFromScientific . refineToScientific is identity on finite values" do
      QuickCheck.property \(value :: Numeric.Numeric precision scale) ->
        case Numeric.refineToScientific value of
          Nothing -> QuickCheck.property True
          Just sci -> Numeric.refineFromScientific @precision @scale sci === Just value

    it "normalizeFromScientific produces projectable values" do
      QuickCheck.property \(sci :: Scientific.Scientific) ->
        let value = Numeric.normalizeFromScientific @precision @scale sci
            normalizedSci = Numeric.normalizeToScientific value
         in Numeric.refineFromScientific @precision @scale normalizedSci === Just value

    when (precision > 0) do
      it "rounds the input scientific to the correct scale" do
        QuickCheck.property \(sci :: Scientific.Scientific) ->
          let value = Numeric.normalizeFromScientific @precision @scale sci
              normalizedSci = Numeric.normalizeToScientific value
              actualScale = negate (Scientific.base10Exponent normalizedSci)
           in actualScale <= scale
