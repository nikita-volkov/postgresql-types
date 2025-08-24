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
import qualified PrimitiveLayer.Types as PrimitiveLayer
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main = hspec do
  testIs @PrimitiveLayer.Inet @(PrimitiveLayer.Ip, Word8) Proxy Proxy
  testIs @PrimitiveLayer.Bit @[Bool] Proxy Proxy
  testIs @PrimitiveLayer.Bit @(VU.Vector Bool) Proxy Proxy
  testIs @PrimitiveLayer.Bool @Bool Proxy Proxy
  testIs @PrimitiveLayer.Bytea @ByteString Proxy Proxy
  testIs @PrimitiveLayer.Float4 @Float Proxy Proxy
  testIs @PrimitiveLayer.Float8 @Double Proxy Proxy
  testIs @PrimitiveLayer.Int2 @Int16 Proxy Proxy
  testIs @PrimitiveLayer.Int4 @Int32 Proxy Proxy
  testIs @PrimitiveLayer.Int8 @Int64 Proxy Proxy
  testIs @PrimitiveLayer.Macaddr @(Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIs @PrimitiveLayer.Macaddr8 @(Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIs @PrimitiveLayer.Money @Int64 Proxy Proxy
  testIs @(PrimitiveLayer.Multirange PrimitiveLayer.Int4) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Int4)) Proxy Proxy
  testIs @(PrimitiveLayer.Multirange PrimitiveLayer.Int8) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Int8)) Proxy Proxy
  testIs @(PrimitiveLayer.Multirange PrimitiveLayer.Numeric) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Numeric)) Proxy Proxy
  testIs @(PrimitiveLayer.Multirange PrimitiveLayer.Timestamp) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Timestamp)) Proxy Proxy
  testIs @(PrimitiveLayer.Multirange PrimitiveLayer.Timestamptz) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Timestamptz)) Proxy Proxy
  testIs @(PrimitiveLayer.Multirange PrimitiveLayer.Date) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Date)) Proxy Proxy
  testIs @PrimitiveLayer.Numeric @(Maybe Scientific.Scientific) Proxy Proxy
  testIs @PrimitiveLayer.Oid @Word32 Proxy Proxy
  testIs @PrimitiveLayer.Point @(Double, Double) Proxy Proxy
  testIs @PrimitiveLayer.Uuid @UUID.UUID Proxy Proxy
  testIs @PrimitiveLayer.Varbit @[Bool] Proxy Proxy
  testIs @PrimitiveLayer.Varbit @(VU.Vector Bool) Proxy Proxy
  testIsMany @PrimitiveLayer.Bool @Bool Proxy Proxy
  testIsMany @PrimitiveLayer.Box @(Double, Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Bytea @ByteString Proxy Proxy
  testIsMany @PrimitiveLayer.Char @Word8 Proxy Proxy
  testIsMany @PrimitiveLayer.Char @Char Proxy Proxy
  testIsMany @PrimitiveLayer.Cidr @(PrimitiveLayer.Ip, Word8) Proxy Proxy
  testIsMany @PrimitiveLayer.Circle @(Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Date @Day Proxy Proxy
  testIsMany @PrimitiveLayer.Float4 @Float Proxy Proxy
  testIsMany @PrimitiveLayer.Float8 @Double Proxy Proxy
  testIsMany @PrimitiveLayer.Int2 @Int16 Proxy Proxy
  testIsMany @PrimitiveLayer.Int4 @Int32 Proxy Proxy
  testIsMany @PrimitiveLayer.Int8 @Int64 Proxy Proxy
  testIsMany @PrimitiveLayer.Interval @(Int32, Int32, Int64) Proxy Proxy
  testIsMany @PrimitiveLayer.IntervalAsMicroseconds @PrimitiveLayer.Interval Proxy Proxy
  testIsMany @PrimitiveLayer.IntervalAsMicroseconds @DiffTime Proxy Proxy
  testIsMany @PrimitiveLayer.Json @Aeson.Value Proxy Proxy
  testIsMany @PrimitiveLayer.Line @(Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Lseg @(Double, Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Money @Int64 Proxy Proxy
  testIsMany @(PrimitiveLayer.Multirange PrimitiveLayer.Int4) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Int4)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Multirange PrimitiveLayer.Int8) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Int8)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Multirange PrimitiveLayer.Numeric) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Numeric)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Multirange PrimitiveLayer.Timestamp) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Timestamp)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Multirange PrimitiveLayer.Timestamptz) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Timestamptz)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Multirange PrimitiveLayer.Date) @(Vector (PrimitiveLayer.Range PrimitiveLayer.Date)) Proxy Proxy
  testIsMany @PrimitiveLayer.Oid @Word32 Proxy Proxy
  testIsMany @PrimitiveLayer.Point @(Double, Double) Proxy Proxy
  testIsMany @(PrimitiveLayer.Range PrimitiveLayer.Int4) @(Maybe (Maybe PrimitiveLayer.Int4, Maybe PrimitiveLayer.Int4)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Range PrimitiveLayer.Int8) @(Maybe (Maybe PrimitiveLayer.Int8, Maybe PrimitiveLayer.Int8)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Range PrimitiveLayer.Numeric) @(Maybe (Maybe PrimitiveLayer.Numeric, Maybe PrimitiveLayer.Numeric)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Range PrimitiveLayer.Timestamp) @(Maybe (Maybe PrimitiveLayer.Timestamp, Maybe PrimitiveLayer.Timestamp)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Range PrimitiveLayer.Timestamptz) @(Maybe (Maybe PrimitiveLayer.Timestamptz, Maybe PrimitiveLayer.Timestamptz)) Proxy Proxy
  testIsMany @(PrimitiveLayer.Range PrimitiveLayer.Date) @(Maybe (Maybe PrimitiveLayer.Date, Maybe PrimitiveLayer.Date)) Proxy Proxy
  testIsMany @PrimitiveLayer.Text @Text.Text Proxy Proxy
  testIsMany @PrimitiveLayer.Time @TimeOfDay Proxy Proxy
  testIsMany @PrimitiveLayer.Timestamp @LocalTime Proxy Proxy
  testIsMany @PrimitiveLayer.Timestamptz @UTCTime Proxy Proxy
  testIsMany @PrimitiveLayer.TimetzAsTimeOfDayAndTimeZone @PrimitiveLayer.Timetz Proxy Proxy
  testIsMany @PrimitiveLayer.TimetzAsTimeOfDayAndTimeZone @(TimeOfDay, TimeZone) Proxy Proxy
  testIsMany @PrimitiveLayer.Uuid @UUID.UUID Proxy Proxy
  testIsMany @PrimitiveLayer.Varchar @Text.Text Proxy Proxy
  testIsMany @Scientific.Scientific @PrimitiveLayer.Numeric Proxy Proxy
  testIsSome @PrimitiveLayer.Path @(Bool, [(Double, Double)]) Proxy Proxy
  testIsSome @PrimitiveLayer.Path @(Bool, (Data.Vector.Unboxed.Vector (Double, Double))) Proxy Proxy
  testIsSome @PrimitiveLayer.Polygon @(Data.Vector.Unboxed.Vector (Double, Double)) Proxy Proxy
  testIsSome @PrimitiveLayer.Polygon @[(Double, Double)] Proxy Proxy

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
