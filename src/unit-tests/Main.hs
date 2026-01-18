module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Scientific as Scientific
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as VU
import Data.Word
import qualified LawfulConversions
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Algebra
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import qualified TextBuilder
import Prelude

main :: IO ()
main = hspec do
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
  testIsScalar @PostgresqlTypes.IntervalAsMicroseconds Proxy
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
  testIs @PostgresqlTypes.Inet @(PostgresqlTypes.Ip, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Bool @Bool Proxy Proxy
  testIs @PostgresqlTypes.Bytea @ByteString Proxy Proxy
  testIs @PostgresqlTypes.Float4 @Float Proxy Proxy
  testIs @PostgresqlTypes.Float8 @Double Proxy Proxy
  testIs @PostgresqlTypes.Int2 @Int16 Proxy Proxy
  testIs @PostgresqlTypes.Int4 @Int32 Proxy Proxy
  testIs @PostgresqlTypes.Int8 @Int64 Proxy Proxy
  testIs @PostgresqlTypes.Lseg @(Double, Double, Double, Double) Proxy Proxy
  testIs @PostgresqlTypes.Macaddr @(Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Macaddr8 @(Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Money @Int64 Proxy Proxy
  testIs @PostgresqlTypes.Oid @Word32 Proxy Proxy
  testIs @PostgresqlTypes.Point @(Double, Double) Proxy Proxy
  testIs @PostgresqlTypes.Uuid @UUID.UUID Proxy Proxy
  testIsSome @(PostgresqlTypes.Varbit 0) @[Bool] Proxy Proxy
  testIsSome @(PostgresqlTypes.Varbit 128) @[Bool] Proxy Proxy
  testIsSome @(PostgresqlTypes.Varbit 128) @(VU.Vector Bool) Proxy Proxy
  testIsMany @(PostgresqlTypes.Varbit 128) @[Bool] Proxy Proxy
  testIsMany @(PostgresqlTypes.Varbit 128) @(VU.Vector Bool) Proxy Proxy
  testIsMany @PostgresqlTypes.Bool @Bool Proxy Proxy
  testIsMany @PostgresqlTypes.Box @(Double, Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Bytea @ByteString Proxy Proxy
  testIsMany @PostgresqlTypes.Char @Word8 Proxy Proxy
  testIsMany @PostgresqlTypes.Char @Char Proxy Proxy
  testIsMany @(PostgresqlTypes.Bpchar 0) @Text Proxy Proxy
  testIsMany @(PostgresqlTypes.Bpchar 1) @Text Proxy Proxy
  testIsMany @(PostgresqlTypes.Bpchar 42) @Text Proxy Proxy
  testIsMany @PostgresqlTypes.Cidr @(PostgresqlTypes.Ip, Word8) Proxy Proxy
  testIsMany @PostgresqlTypes.Circle @(Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Date @Day Proxy Proxy
  testIsMany @PostgresqlTypes.Float4 @Float Proxy Proxy
  testIsMany @PostgresqlTypes.Float8 @Double Proxy Proxy
  testIsMany @PostgresqlTypes.Int2 @Int16 Proxy Proxy
  testIsMany @PostgresqlTypes.Int4 @Int32 Proxy Proxy
  testIsMany @PostgresqlTypes.Int8 @Int64 Proxy Proxy
  testIsMany @PostgresqlTypes.Interval @(Int32, Int32, Int64) Proxy Proxy
  testIsMany @PostgresqlTypes.IntervalAsMicroseconds @PostgresqlTypes.Interval Proxy Proxy
  testIsMany @PostgresqlTypes.IntervalAsMicroseconds @DiffTime Proxy Proxy
  testIsMany @PostgresqlTypes.Json @Aeson.Value Proxy Proxy
  testIsMany @PostgresqlTypes.Line @(Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Money @Int64 Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Int4) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Int4)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Int8) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Int8)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange (PostgresqlTypes.Numeric 0 0)) @(Vector (PostgresqlTypes.Range (PostgresqlTypes.Numeric 0 0))) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamp) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Timestamp)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamptz) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Timestamptz)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Date) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Date)) Proxy Proxy
  testIsMany @PostgresqlTypes.Oid @Word32 Proxy Proxy
  testIsMany @PostgresqlTypes.Point @(Double, Double) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Int4) @(Maybe (Maybe PostgresqlTypes.Int4, Maybe PostgresqlTypes.Int4)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Int8) @(Maybe (Maybe PostgresqlTypes.Int8, Maybe PostgresqlTypes.Int8)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range (PostgresqlTypes.Numeric 0 0)) @(Maybe (Maybe (PostgresqlTypes.Numeric 0 0), Maybe (PostgresqlTypes.Numeric 0 0))) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Timestamp) @(Maybe (Maybe PostgresqlTypes.Timestamp, Maybe PostgresqlTypes.Timestamp)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Timestamptz) @(Maybe (Maybe PostgresqlTypes.Timestamptz, Maybe PostgresqlTypes.Timestamptz)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Date) @(Maybe (Maybe PostgresqlTypes.Date, Maybe PostgresqlTypes.Date)) Proxy Proxy
  testIsMany @PostgresqlTypes.Hstore @(Map.Map Text.Text (Maybe Text.Text)) Proxy Proxy
  testIsMany @PostgresqlTypes.Text @Text.Text Proxy Proxy
  testIsMany @PostgresqlTypes.Time @TimeOfDay Proxy Proxy
  testIsMany @PostgresqlTypes.Timestamp @LocalTime Proxy Proxy
  testIsMany @PostgresqlTypes.Timestamptz @UTCTime Proxy Proxy
  testIsMany @PostgresqlTypes.TimetzAsTimeOfDayAndTimeZone @PostgresqlTypes.Timetz Proxy Proxy
  testIsMany @PostgresqlTypes.TimetzAsTimeOfDayAndTimeZone @(TimeOfDay, TimeZone) Proxy Proxy
  testIsMany @PostgresqlTypes.Uuid @UUID.UUID Proxy Proxy
  testIsMany @(PostgresqlTypes.Varchar 0) @Text.Text Proxy Proxy
  testIsMany @(PostgresqlTypes.Varchar 255) @Text.Text Proxy Proxy
  testIsMany @Scientific.Scientific @(PostgresqlTypes.Numeric 0 0) Proxy Proxy
  testIsSome @PostgresqlTypes.Path @(Bool, [(Double, Double)]) Proxy Proxy
  testIsSome @PostgresqlTypes.Path @(Bool, (Data.Vector.Unboxed.Vector (Double, Double))) Proxy Proxy
  testIsSome @PostgresqlTypes.Polygon @(Data.Vector.Unboxed.Vector (Double, Double)) Proxy Proxy
  testIsSome @PostgresqlTypes.Polygon @[(Double, Double)] Proxy Proxy
  testIsSomeBounded @PostgresqlTypes.Time @TimeOfDay Proxy Proxy
  testIsSomeBounded @PostgresqlTypes.TimetzAsTimeOfDayAndTimeZone @(TimeOfDay, TimeZone) Proxy Proxy

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
                    let encoded = PtrPoker.Write.writeToByteString (binEnc value)
                        decoding = PtrPeeker.runVariableOnByteString binDec encoded
                     in decoding === Right (Right value)
