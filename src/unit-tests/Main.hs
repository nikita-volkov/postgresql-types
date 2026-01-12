module Main (main) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.Proxy
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as VU
import Data.Word
import qualified LawfulConversions
import qualified PostgresqlTypes.Primitive.Types as PostgresqlTypes.Primitive
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main = hspec do
  testIs @PostgresqlTypes.Primitive.Inet @(PostgresqlTypes.Primitive.Ip, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Bit @[Bool] Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Bit @(VU.Vector Bool) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Bool @Bool Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Bytea @ByteString Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Float4 @Float Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Float8 @Double Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Int2 @Int16 Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Int4 @Int32 Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Int8 @Int64 Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Lseg @(Double, Double, Double, Double) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Macaddr @(Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Macaddr8 @(Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Money @Int64 Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Numeric @(Maybe Scientific.Scientific) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Oid @Word32 Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Point @(Double, Double) Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Uuid @UUID.UUID Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Varbit @[Bool] Proxy Proxy
  testIs @PostgresqlTypes.Primitive.Varbit @(VU.Vector Bool) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Bool @Bool Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Box @(Double, Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Bytea @ByteString Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Char @Word8 Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Char @Char Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Cidr @(PostgresqlTypes.Primitive.Ip, Word8) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Circle @(Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Date @Day Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Float4 @Float Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Float8 @Double Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Int2 @Int16 Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Int4 @Int32 Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Int8 @Int64 Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Interval @(Int32, Int32, Int64) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.IntervalAsMicroseconds @PostgresqlTypes.Primitive.Interval Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.IntervalAsMicroseconds @DiffTime Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Json @Aeson.Value Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Line @(Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Money @Int64 Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Multirange PostgresqlTypes.Primitive.Int4) @(Vector (PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Int4)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Multirange PostgresqlTypes.Primitive.Int8) @(Vector (PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Int8)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Multirange PostgresqlTypes.Primitive.Numeric) @(Vector (PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Numeric)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Multirange PostgresqlTypes.Primitive.Timestamp) @(Vector (PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Timestamp)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Multirange PostgresqlTypes.Primitive.Timestamptz) @(Vector (PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Timestamptz)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Multirange PostgresqlTypes.Primitive.Date) @(Vector (PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Date)) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Oid @Word32 Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Point @(Double, Double) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Int4) @(Maybe (Maybe PostgresqlTypes.Primitive.Int4, Maybe PostgresqlTypes.Primitive.Int4)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Int8) @(Maybe (Maybe PostgresqlTypes.Primitive.Int8, Maybe PostgresqlTypes.Primitive.Int8)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Numeric) @(Maybe (Maybe PostgresqlTypes.Primitive.Numeric, Maybe PostgresqlTypes.Primitive.Numeric)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Timestamp) @(Maybe (Maybe PostgresqlTypes.Primitive.Timestamp, Maybe PostgresqlTypes.Primitive.Timestamp)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Timestamptz) @(Maybe (Maybe PostgresqlTypes.Primitive.Timestamptz, Maybe PostgresqlTypes.Primitive.Timestamptz)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Primitive.Range PostgresqlTypes.Primitive.Date) @(Maybe (Maybe PostgresqlTypes.Primitive.Date, Maybe PostgresqlTypes.Primitive.Date)) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Text @Text.Text Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Time @TimeOfDay Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Timestamp @LocalTime Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Timestamptz @UTCTime Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.TimetzAsTimeOfDayAndTimeZone @PostgresqlTypes.Primitive.Timetz Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.TimetzAsTimeOfDayAndTimeZone @(TimeOfDay, TimeZone) Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Uuid @UUID.UUID Proxy Proxy
  testIsMany @PostgresqlTypes.Primitive.Varchar @Text.Text Proxy Proxy
  testIsMany @Scientific.Scientific @PostgresqlTypes.Primitive.Numeric Proxy Proxy
  testIsSome @PostgresqlTypes.Primitive.Path @(Bool, [(Double, Double)]) Proxy Proxy
  testIsSome @PostgresqlTypes.Primitive.Path @(Bool, (Data.Vector.Unboxed.Vector (Double, Double))) Proxy Proxy
  testIsSome @PostgresqlTypes.Primitive.Polygon @(Data.Vector.Unboxed.Vector (Double, Double)) Proxy Proxy
  testIsSome @PostgresqlTypes.Primitive.Polygon @[(Double, Double)] Proxy Proxy
  testIsSomeBounded @PostgresqlTypes.Primitive.Time @TimeOfDay Proxy Proxy
  testIsSomeBounded @PostgresqlTypes.Primitive.TimetzAsTimeOfDayAndTimeZone @(TimeOfDay, TimeZone) Proxy Proxy

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
