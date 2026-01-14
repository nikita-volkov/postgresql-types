module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.Proxy
import qualified Data.Scientific as Scientific
import Data.Tagged
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as VU
import Data.Word
import qualified LawfulConversions
import qualified PostgresqlTypes as PostgresqlTypes
import qualified PtrPeeker
import qualified PtrPoker.Write
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary, (===))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()
import qualified TextBuilder
import Prelude

main :: IO ()
main = hspec do
  testIsStandardType @PostgresqlTypes.Bit Proxy
  testIsStandardType @PostgresqlTypes.Bool Proxy
  testIsStandardType @PostgresqlTypes.Box Proxy
  testIsStandardType @PostgresqlTypes.Bytea Proxy
  testIsStandardType @PostgresqlTypes.Char Proxy
  testIsStandardType @PostgresqlTypes.Cidr Proxy
  testIsStandardType @PostgresqlTypes.Circle Proxy
  testIsStandardType @PostgresqlTypes.Date Proxy
  testIsStandardType @PostgresqlTypes.Float4 Proxy
  testIsStandardType @PostgresqlTypes.Float8 Proxy
  testIsStandardType @PostgresqlTypes.Inet Proxy
  testIsStandardType @PostgresqlTypes.Int2 Proxy
  testIsStandardType @PostgresqlTypes.Int4 Proxy
  testIsStandardType @PostgresqlTypes.Int8 Proxy
  testIsStandardType @PostgresqlTypes.Interval Proxy
  testIsStandardType @PostgresqlTypes.IntervalAsMicroseconds Proxy
  testIsStandardType @PostgresqlTypes.Json Proxy
  testIsStandardType @PostgresqlTypes.Jsonb Proxy
  testIsStandardType @PostgresqlTypes.Line Proxy
  testIsStandardType @PostgresqlTypes.Lseg Proxy
  testIsStandardType @PostgresqlTypes.Macaddr Proxy
  testIsStandardType @PostgresqlTypes.Macaddr8 Proxy
  testIsStandardType @PostgresqlTypes.Money Proxy
  testIsStandardType @PostgresqlTypes.Numeric Proxy
  testIsStandardType @PostgresqlTypes.Oid Proxy
  testIsStandardType @PostgresqlTypes.Path Proxy
  testIsStandardType @PostgresqlTypes.Point Proxy
  testIsStandardType @PostgresqlTypes.Polygon Proxy
  testIsStandardType @(PostgresqlTypes.Range PostgresqlTypes.Int4) Proxy
  testIsStandardType @(PostgresqlTypes.Range PostgresqlTypes.Int8) Proxy
  testIsStandardType @(PostgresqlTypes.Range PostgresqlTypes.Numeric) Proxy
  testIsStandardType @(PostgresqlTypes.Range PostgresqlTypes.Timestamp) Proxy
  testIsStandardType @(PostgresqlTypes.Range PostgresqlTypes.Timestamptz) Proxy
  testIsStandardType @(PostgresqlTypes.Range PostgresqlTypes.Date) Proxy
  testIsStandardType @(PostgresqlTypes.Multirange PostgresqlTypes.Int4) Proxy
  testIsStandardType @(PostgresqlTypes.Multirange PostgresqlTypes.Int8) Proxy
  testIsStandardType @(PostgresqlTypes.Multirange PostgresqlTypes.Numeric) Proxy
  testIsStandardType @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamp) Proxy
  testIsStandardType @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamptz) Proxy
  testIsStandardType @(PostgresqlTypes.Multirange PostgresqlTypes.Date) Proxy
  testIsStandardType @PostgresqlTypes.Text Proxy
  testIsStandardType @PostgresqlTypes.Time Proxy
  testIsStandardType @PostgresqlTypes.Timestamp Proxy
  testIsStandardType @PostgresqlTypes.Timestamptz Proxy
  testIsStandardType @PostgresqlTypes.Timetz Proxy
  testIsStandardType @PostgresqlTypes.Uuid Proxy
  testIsStandardType @PostgresqlTypes.Varbit Proxy
  testIsStandardType @PostgresqlTypes.Varchar Proxy
  testIs @PostgresqlTypes.Inet @(PostgresqlTypes.Ip, Word8) Proxy Proxy
  testIs @PostgresqlTypes.Bit @[Bool] Proxy Proxy
  testIs @PostgresqlTypes.Bit @(VU.Vector Bool) Proxy Proxy
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
  testIs @PostgresqlTypes.Numeric @(Maybe Scientific.Scientific) Proxy Proxy
  testIs @PostgresqlTypes.Oid @Word32 Proxy Proxy
  testIs @PostgresqlTypes.Point @(Double, Double) Proxy Proxy
  testIs @PostgresqlTypes.Uuid @UUID.UUID Proxy Proxy
  testIs @PostgresqlTypes.Varbit @[Bool] Proxy Proxy
  testIs @PostgresqlTypes.Varbit @(VU.Vector Bool) Proxy Proxy
  testIsMany @PostgresqlTypes.Bool @Bool Proxy Proxy
  testIsMany @PostgresqlTypes.Box @(Double, Double, Double, Double) Proxy Proxy
  testIsMany @PostgresqlTypes.Bytea @ByteString Proxy Proxy
  testIsMany @PostgresqlTypes.Char @Word8 Proxy Proxy
  testIsMany @PostgresqlTypes.Char @Char Proxy Proxy
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
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Numeric) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Numeric)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamp) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Timestamp)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Timestamptz) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Timestamptz)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Multirange PostgresqlTypes.Date) @(Vector (PostgresqlTypes.Range PostgresqlTypes.Date)) Proxy Proxy
  testIsMany @PostgresqlTypes.Oid @Word32 Proxy Proxy
  testIsMany @PostgresqlTypes.Point @(Double, Double) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Int4) @(Maybe (Maybe PostgresqlTypes.Int4, Maybe PostgresqlTypes.Int4)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Int8) @(Maybe (Maybe PostgresqlTypes.Int8, Maybe PostgresqlTypes.Int8)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Numeric) @(Maybe (Maybe PostgresqlTypes.Numeric, Maybe PostgresqlTypes.Numeric)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Timestamp) @(Maybe (Maybe PostgresqlTypes.Timestamp, Maybe PostgresqlTypes.Timestamp)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Timestamptz) @(Maybe (Maybe PostgresqlTypes.Timestamptz, Maybe PostgresqlTypes.Timestamptz)) Proxy Proxy
  testIsMany @(PostgresqlTypes.Range PostgresqlTypes.Date) @(Maybe (Maybe PostgresqlTypes.Date, Maybe PostgresqlTypes.Date)) Proxy Proxy
  testIsMany @PostgresqlTypes.Text @Text.Text Proxy Proxy
  testIsMany @PostgresqlTypes.Time @TimeOfDay Proxy Proxy
  testIsMany @PostgresqlTypes.Timestamp @LocalTime Proxy Proxy
  testIsMany @PostgresqlTypes.Timestamptz @UTCTime Proxy Proxy
  testIsMany @PostgresqlTypes.TimetzAsTimeOfDayAndTimeZone @PostgresqlTypes.Timetz Proxy Proxy
  testIsMany @PostgresqlTypes.TimetzAsTimeOfDayAndTimeZone @(TimeOfDay, TimeZone) Proxy Proxy
  testIsMany @PostgresqlTypes.Uuid @UUID.UUID Proxy Proxy
  testIsMany @PostgresqlTypes.Varchar @Text.Text Proxy Proxy
  testIsMany @Scientific.Scientific @PostgresqlTypes.Numeric Proxy Proxy
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
testIsStandardType ::
  forall a.
  ( QuickCheck.Arbitrary a,
    Show a,
    Eq a,
    PostgresqlTypes.IsStandardType a,
    Typeable a
  ) =>
  Proxy a ->
  Spec
testIsStandardType _ =
  let typeName = untag (PostgresqlTypes.typeName @a)
      binEnc = PostgresqlTypes.binaryEncoder @a
      binDec = PostgresqlTypes.binaryDecoder @a
      txtEnc = PostgresqlTypes.textualEncoder @a
      txtDec = PostgresqlTypes.textualDecoder @a
   in describe (show (typeOf (undefined :: a))) do
        describe (Text.unpack typeName) do
          describe "IsStandardType" do
            describe "Encoding via textualEncoder" do
              describe "And decoding via textualDecoder" do
                it "Should produce the original value" $
                  QuickCheck.property \(value :: a) ->
                    let encoded = TextBuilder.toText (txtEnc value)
                        decoding = Data.Attoparsec.Text.parseOnly txtDec encoded
                     in decoding === Right value

            describe "Encoding via binaryEncoder" do
              describe "And decoding via binaryDecoder" do
                it "Should produce the original value" $
                  QuickCheck.property \(value :: a) ->
                    let encoded = PtrPoker.Write.writeToByteString (binEnc value)
                        decoding = PtrPeeker.runVariableOnByteString binDec encoded
                     in decoding === Right (Right value)
