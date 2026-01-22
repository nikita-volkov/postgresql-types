module Main (main) where

import qualified CidrSpec
import qualified Data.Attoparsec.Text
import Data.Proxy
import Data.Tagged
import qualified Data.Text as Text
import Data.Typeable
import qualified InetSpec
import qualified IntervalSpec
import qualified NumericSpec
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Algebra
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import qualified TimetzSpec
import Prelude

main :: IO ()
main = hspec $ parallel do
  testIsScalar @(PostgresqlTypes.Bit 0) Proxy
  testIsScalar @(PostgresqlTypes.Bit 1) Proxy
  testIsScalar @(PostgresqlTypes.Bit 64) Proxy
  testIsScalar @PostgresqlTypes.Bool Proxy
  testIsScalar @PostgresqlTypes.Box Proxy
  testIsScalar @PostgresqlTypes.Bytea Proxy
  testIsScalar @PostgresqlTypes.Char Proxy
  testIsScalar @(PostgresqlTypes.Bpchar 1) Proxy
  testIsScalar @(PostgresqlTypes.Bpchar 42) Proxy
  testIsScalar @PostgresqlTypes.Cidr Proxy
  testIsScalar @PostgresqlTypes.Circle Proxy
  testIsScalar @PostgresqlTypes.Date Proxy
  testIsScalar @PostgresqlTypes.Float4 Proxy
  testIsScalar @PostgresqlTypes.Float8 Proxy
  testIsScalar @PostgresqlTypes.Hstore Proxy
  testIsScalar @PostgresqlTypes.Inet Proxy
  testIsScalar @PostgresqlTypes.Int2 Proxy
  testIsScalar @PostgresqlTypes.Int4 Proxy
  testIsScalar @PostgresqlTypes.Int8 Proxy
  testIsScalar @PostgresqlTypes.Interval Proxy
  testIsScalar @PostgresqlTypes.Json Proxy
  testIsScalar @PostgresqlTypes.Jsonb Proxy
  testIsScalar @PostgresqlTypes.Line Proxy
  testIsScalar @PostgresqlTypes.Lseg Proxy
  testIsScalar @PostgresqlTypes.Macaddr Proxy
  testIsScalar @PostgresqlTypes.Macaddr8 Proxy
  testIsScalar @PostgresqlTypes.Money Proxy
  testIsScalar @(PostgresqlTypes.Numeric 0 0) Proxy
  testIsScalar @(PostgresqlTypes.Numeric 10 0) Proxy
  testIsScalar @(PostgresqlTypes.Numeric 10 2) Proxy
  testIsScalar @(PostgresqlTypes.Numeric 10 7) Proxy
  testIsScalar @PostgresqlTypes.Oid Proxy
  testIsScalar @PostgresqlTypes.Path Proxy
  testIsScalar @PostgresqlTypes.Point Proxy
  testIsScalar @PostgresqlTypes.Polygon Proxy
  testIsScalar @(PostgresqlTypes.Range PostgresqlTypes.Int4) Proxy
  testIsScalar @(PostgresqlTypes.Range PostgresqlTypes.Int8) Proxy
  testIsScalar @(PostgresqlTypes.Range (PostgresqlTypes.Numeric 0 0)) Proxy
  testIsScalar @(PostgresqlTypes.Range PostgresqlTypes.Timestamp) Proxy
  testIsScalar @(PostgresqlTypes.Range PostgresqlTypes.Timestamptz) Proxy
  testIsScalar @(PostgresqlTypes.Range PostgresqlTypes.Date) Proxy
  testIsScalar @(PostgresqlTypes.Multirange PostgresqlTypes.Int4) Proxy
  testIsScalar @(PostgresqlTypes.Multirange PostgresqlTypes.Int8) Proxy
  testIsScalar @(PostgresqlTypes.Multirange (PostgresqlTypes.Numeric 0 0)) Proxy
  testIsScalar @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamp) Proxy
  testIsScalar @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamptz) Proxy
  testIsScalar @(PostgresqlTypes.Multirange PostgresqlTypes.Date) Proxy
  testIsScalar @PostgresqlTypes.Text Proxy
  testIsScalar @PostgresqlTypes.Time Proxy
  testIsScalar @PostgresqlTypes.Timestamp Proxy
  testIsScalar @PostgresqlTypes.Timestamptz Proxy
  testIsScalar @PostgresqlTypes.Timetz Proxy
  testIsScalar @PostgresqlTypes.Uuid Proxy
  testIsScalar @(PostgresqlTypes.Varbit 128) Proxy
  testIsScalar @(PostgresqlTypes.Varchar 255) Proxy
  -- Lawful-conversions tests removed as we migrated to type-specific APIs
  describe "Cidr" CidrSpec.spec
  describe "Inet" InetSpec.spec
  describe "Interval" IntervalSpec.spec
  describe "Numeric" NumericSpec.spec
  describe "Timetz" TimetzSpec.spec

-- Lawful-conversions test functions removed as we migrated to type-specific APIs
{-
-- | Test lawful conversions for a PostgreSQL type
testIsMany ::
  forall primitive projection.
  ( HasCallStack,
    LawfulConversions.IsMany projection primitive,
    Typeable projection,
    Typeable primitive,
    Eq projection,
    Eq primitive,
    Show projection,
    Show primitive,
    Arbitrary projection,
    Arbitrary primitive
  ) =>
  Proxy projection -> Proxy primitive -> Spec
testIsMany projection primitive =
  describe (show (typeOf (undefined :: primitive))) do
    describe (show (typeOf (undefined :: projection))) do
      describe "IsMany" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isManyProperties projection primitive)
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties projection primitive)

-- | Test lawful conversions for a PostgreSQL type
testIsSome ::
  forall primitive projection.
  ( HasCallStack,
    LawfulConversions.IsSome projection primitive,
    Typeable projection,
    Typeable primitive,
    Eq projection,
    Eq primitive,
    Show projection,
    Show primitive,
    Arbitrary projection,
    Arbitrary primitive
  ) =>
  Proxy projection -> Proxy primitive -> Spec
testIsSome projection primitive =
  describe (show (typeOf (undefined :: primitive))) do
    describe (show (typeOf (undefined :: projection))) do
      describe "IsSome" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isSomeProperties projection primitive)

-- | Test lawful conversions for a PostgreSQL type
testIsSomeBounded ::
  forall primitive projection.
  ( HasCallStack,
    LawfulConversions.IsSome projection primitive,
    Bounded primitive,
    Typeable projection,
    Typeable primitive,
    Eq projection,
    Eq primitive,
    Show projection,
    Show primitive,
    Arbitrary projection,
    Arbitrary primitive
  ) =>
  Proxy projection -> Proxy primitive -> Spec
testIsSomeBounded _projection _primitive =
  describe (show (typeOf (undefined :: primitive))) do
    describe (show (typeOf (undefined :: projection))) do
      describe "Bounded" do
        describe "minBound" do
          it "is convertible" do
            let projection = LawfulConversions.to @projection (minBound @primitive)
                restoration = LawfulConversions.maybeTo @primitive projection
            shouldBe restoration (Just (minBound @primitive))
        describe "maxBound" do
          it "is convertible" do
            let projection = LawfulConversions.to @projection (maxBound @primitive)
                restoration = LawfulConversions.maybeTo @primitive projection
            shouldBe restoration (Just (maxBound @primitive))

-- | Test lawful conversions for a PostgreSQL type
testIs ::
  forall primitive projection.
  ( HasCallStack,
    LawfulConversions.Is projection primitive,
    Typeable projection,
    Typeable primitive,
    Eq projection,
    Eq primitive,
    Show projection,
    Show primitive,
    Arbitrary projection,
    Arbitrary primitive
  ) =>
  Proxy projection -> Proxy primitive -> Spec
testIs projection primitive =
  describe (show (typeOf (undefined :: primitive))) do
    describe (show (typeOf (undefined :: projection))) do
      describe "Is" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isProperties projection primitive)

      describe "Mirror Is" do
        traverse_
          (uncurry prop)
          (LawfulConversions.isProperties primitive projection)
-}

-- | Test textual encoder/decoder roundtrip
testIsScalar ::
  forall a.
  ( QuickCheck.Arbitrary a,
    Show a,
    Eq a,
    PostgresqlTypes.Algebra.IsScalar a,
    Typeable a
  ) =>
  Proxy a ->
  Spec
testIsScalar _ =
  let typeName = untag (PostgresqlTypes.Algebra.typeName @a)
      binEnc = PostgresqlTypes.Algebra.binaryEncoder @a
      binDec = PostgresqlTypes.Algebra.binaryDecoder @a
      txtEnc = PostgresqlTypes.Algebra.textualEncoder @a
      txtDec = PostgresqlTypes.Algebra.textualDecoder @a
   in describe (show (typeOf (undefined :: a))) do
        describe (Text.unpack typeName) do
          describe "IsScalar" do
            describe "Encoding via textualEncoder" do
              describe "And decoding via textualDecoder" do
                it "Should produce the original value" $
                  QuickCheck.property \(value :: a) ->
                    let encoded = TextBuilder.toText (txtEnc value)
                        decoding = Data.Attoparsec.Text.parseOnly txtDec encoded
                     in counterexample ("Encoded: " ++ show encoded) $
                          decoding === Right value

            describe "Encoding via binaryEncoder" do
              describe "And decoding via binaryDecoder" do
                it "Should produce the original value" $
                  QuickCheck.property \(value :: a) ->
                    let encoded = PtrPoker.Write.toByteString (binEnc value)
                        decoding = PtrPeeker.runVariableOnByteString binDec encoded
                     in decoding === Right (Right value)
