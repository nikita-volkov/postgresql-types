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
import Data.Word
import qualified LawfulConversions
import qualified PrimitiveLayer
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main = hspec do
  testIsMany @PrimitiveLayer.Bit @[Bool] Proxy Proxy
  testIsMany @PrimitiveLayer.Bool @Bool Proxy Proxy
  testIsMany @PrimitiveLayer.Box @(Double, Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Bytea @ByteString Proxy Proxy
  testIsMany @PrimitiveLayer.Char @Word8 Proxy Proxy
  testIsMany @PrimitiveLayer.Char @Char Proxy Proxy
  testIsMany @PrimitiveLayer.Cidr @(PrimitiveLayer.CidrIpAddress, Word8) Proxy Proxy
  testIsMany @PrimitiveLayer.Circle @(Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Date @Day Proxy Proxy
  testIsMany @PrimitiveLayer.Float4 @Float Proxy Proxy
  testIsMany @PrimitiveLayer.Float8 @Double Proxy Proxy
  testIsMany @PrimitiveLayer.Int2 @Int16 Proxy Proxy
  testIsMany @PrimitiveLayer.Int4 @Int32 Proxy Proxy
  testIsMany @PrimitiveLayer.Int8 @Int64 Proxy Proxy
  testIsMany @PrimitiveLayer.Interval @(Int32, Int32, Int64) Proxy Proxy
  testIsMany @PrimitiveLayer.IntervalInMicroseconds @PrimitiveLayer.Interval Proxy Proxy
  testIsMany @PrimitiveLayer.IntervalInMicroseconds @DiffTime Proxy Proxy
  testIsMany @PrimitiveLayer.Json @Aeson.Value Proxy Proxy
  testIsMany @PrimitiveLayer.Line @(Double, Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Lseg @((Double, Double), (Double, Double)) Proxy Proxy
  testIsMany @PrimitiveLayer.Macaddr8 @(Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Proxy Proxy
  testIsMany @PrimitiveLayer.Money @Int64 Proxy Proxy
  testIsMany @PrimitiveLayer.Numeric @Scientific.Scientific Proxy Proxy
  testIsMany @PrimitiveLayer.Oid @Word32 Proxy Proxy
  testIsMany @PrimitiveLayer.Path @(Bool, [(Double, Double)]) Proxy Proxy
  testIsMany @PrimitiveLayer.Point @(Double, Double) Proxy Proxy
  testIsMany @PrimitiveLayer.Polygon @[(Double, Double)] Proxy Proxy
  testIsMany @PrimitiveLayer.Text @Text.Text Proxy Proxy
  testIsMany @PrimitiveLayer.Time @TimeOfDay Proxy Proxy
  testIsMany @PrimitiveLayer.Timestamp @LocalTime Proxy Proxy
  testIsMany @PrimitiveLayer.Timestamptz @UTCTime Proxy Proxy
  testIsMany @PrimitiveLayer.Uuid @UUID.UUID Proxy Proxy
  testIsMany @PrimitiveLayer.Varbit @[Bool] Proxy Proxy
  testIsMany @PrimitiveLayer.Xml @Text.Text Proxy Proxy

-- | Test lawful conversions for a PostgreSQL type
testIsMany ::
  forall primitive projection.
  ( LawfulConversions.IsMany projection primitive,
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
