module Main (main) where

import qualified BitSpec
import qualified BoolSpec
import qualified BoxSpec
import qualified BpcharSpec
import qualified ByteaSpec
import qualified CharSpec
import qualified CidrSpec
import qualified CircleSpec
import qualified Data.Attoparsec.Text
import Data.Proxy
import Data.Tagged
import qualified Data.Text as Text
import Data.Typeable
import qualified DateSpec
import qualified Float4Spec
import qualified Float8Spec
import qualified HstoreSpec
import qualified InetSpec
import qualified Int2Spec
import qualified Int4Spec
import qualified Int8Spec
import qualified IntervalSpec
import qualified JsonSpec
import qualified JsonbSpec
import qualified LineSpec
import qualified LsegSpec
import qualified Macaddr8Spec
import qualified MacaddrSpec
import qualified MoneySpec
import qualified MultirangeSpec
import qualified NumericSpec
import qualified OidSpec
import qualified PathSpec
import qualified PointSpec
import qualified PolygonSpec
import qualified PostgresqlTypes
import qualified PostgresqlTypes.Algebra
import qualified PtrPeeker
import qualified PtrPoker.Write
import qualified RangeSpec
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import qualified TextSpec
import qualified TimeSpec
import qualified TimestampSpec
import qualified TimestamptzSpec
import qualified TimetzSpec
import qualified UuidSpec
import qualified VarbitSpec
import qualified VarcharSpec
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
  describe "Bit" BitSpec.spec
  describe "Bool" BoolSpec.spec
  describe "Box" BoxSpec.spec
  describe "Bpchar" BpcharSpec.spec
  describe "Bytea" ByteaSpec.spec
  describe "Char" CharSpec.spec
  describe "Cidr" CidrSpec.spec
  describe "Circle" CircleSpec.spec
  describe "Date" DateSpec.spec
  describe "Float4" Float4Spec.spec
  describe "Float8" Float8Spec.spec
  describe "Hstore" HstoreSpec.spec
  describe "Inet" InetSpec.spec
  describe "Int2" Int2Spec.spec
  describe "Int4" Int4Spec.spec
  describe "Int8" Int8Spec.spec
  describe "Interval" IntervalSpec.spec
  describe "Json" JsonSpec.spec
  describe "Jsonb" JsonbSpec.spec
  describe "Line" LineSpec.spec
  describe "Lseg" LsegSpec.spec
  describe "Macaddr" MacaddrSpec.spec
  describe "Macaddr8" Macaddr8Spec.spec
  describe "Money" MoneySpec.spec
  describe "Multirange" MultirangeSpec.spec
  describe "Numeric" NumericSpec.spec
  describe "Oid" OidSpec.spec
  describe "Path" PathSpec.spec
  describe "Point" PointSpec.spec
  describe "Polygon" PolygonSpec.spec
  describe "Range" RangeSpec.spec
  describe "Text" TextSpec.spec
  describe "Time" TimeSpec.spec
  describe "Timestamp" TimestampSpec.spec
  describe "Timestamptz" TimestamptzSpec.spec
  describe "Timetz" TimetzSpec.spec
  describe "Uuid" UuidSpec.spec
  describe "Varbit" VarbitSpec.spec
  describe "Varchar" VarcharSpec.spec

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
