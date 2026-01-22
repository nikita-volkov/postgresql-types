module Main (main) where

import Data.Proxy
import qualified PostgresqlTypes
import qualified PostgresqlTypes.BitSpec
import qualified PostgresqlTypes.BoolSpec
import qualified PostgresqlTypes.BoxSpec
import qualified PostgresqlTypes.BpcharSpec
import qualified PostgresqlTypes.ByteaSpec
import qualified PostgresqlTypes.CharSpec
import qualified PostgresqlTypes.CidrSpec
import qualified PostgresqlTypes.CircleSpec
import qualified PostgresqlTypes.DateSpec
import qualified PostgresqlTypes.Float4Spec
import qualified PostgresqlTypes.Float8Spec
import qualified PostgresqlTypes.HstoreSpec
import qualified PostgresqlTypes.InetSpec
import qualified PostgresqlTypes.Int2Spec
import qualified PostgresqlTypes.Int4Spec
import qualified PostgresqlTypes.Int8Spec
import qualified PostgresqlTypes.IntervalSpec
import qualified PostgresqlTypes.JsonSpec
import qualified PostgresqlTypes.JsonbSpec
import qualified PostgresqlTypes.LineSpec
import qualified PostgresqlTypes.LsegSpec
import qualified PostgresqlTypes.Macaddr8Spec
import qualified PostgresqlTypes.MacaddrSpec
import qualified PostgresqlTypes.MoneySpec
import qualified PostgresqlTypes.MultirangeSpec
import qualified PostgresqlTypes.NumericSpec
import qualified PostgresqlTypes.OidSpec
import qualified PostgresqlTypes.PathSpec
import qualified PostgresqlTypes.PointSpec
import qualified PostgresqlTypes.PolygonSpec
import qualified PostgresqlTypes.RangeSpec
import qualified PostgresqlTypes.TextSpec
import qualified PostgresqlTypes.TimeSpec
import qualified PostgresqlTypes.TimestampSpec
import qualified PostgresqlTypes.TimestamptzSpec
import qualified PostgresqlTypes.TimetzSpec
import qualified PostgresqlTypes.UuidSpec
import qualified PostgresqlTypes.VarbitSpec
import qualified PostgresqlTypes.VarcharSpec
import Test.Hspec
import UnitTests.Scripts (testIsScalar)
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
  describe "Bit" PostgresqlTypes.BitSpec.spec
  describe "Bool" PostgresqlTypes.BoolSpec.spec
  describe "Box" PostgresqlTypes.BoxSpec.spec
  describe "Bpchar" PostgresqlTypes.BpcharSpec.spec
  describe "Bytea" PostgresqlTypes.ByteaSpec.spec
  describe "Char" PostgresqlTypes.CharSpec.spec
  describe "Cidr" PostgresqlTypes.CidrSpec.spec
  describe "Circle" PostgresqlTypes.CircleSpec.spec
  describe "Date" PostgresqlTypes.DateSpec.spec
  describe "Float4" PostgresqlTypes.Float4Spec.spec
  describe "Float8" PostgresqlTypes.Float8Spec.spec
  describe "Hstore" PostgresqlTypes.HstoreSpec.spec
  describe "Inet" PostgresqlTypes.InetSpec.spec
  describe "Int2" PostgresqlTypes.Int2Spec.spec
  describe "Int4" PostgresqlTypes.Int4Spec.spec
  describe "Int8" PostgresqlTypes.Int8Spec.spec
  describe "Interval" PostgresqlTypes.IntervalSpec.spec
  describe "Json" PostgresqlTypes.JsonSpec.spec
  describe "Jsonb" PostgresqlTypes.JsonbSpec.spec
  describe "Line" PostgresqlTypes.LineSpec.spec
  describe "Lseg" PostgresqlTypes.LsegSpec.spec
  describe "Macaddr" PostgresqlTypes.MacaddrSpec.spec
  describe "Macaddr8" PostgresqlTypes.Macaddr8Spec.spec
  describe "Money" PostgresqlTypes.MoneySpec.spec
  describe "Multirange" PostgresqlTypes.MultirangeSpec.spec
  describe "Numeric" PostgresqlTypes.NumericSpec.spec
  describe "Oid" PostgresqlTypes.OidSpec.spec
  describe "Path" PostgresqlTypes.PathSpec.spec
  describe "Point" PostgresqlTypes.PointSpec.spec
  describe "Polygon" PostgresqlTypes.PolygonSpec.spec
  describe "Range" PostgresqlTypes.RangeSpec.spec
  describe "Text" PostgresqlTypes.TextSpec.spec
  describe "Time" PostgresqlTypes.TimeSpec.spec
  describe "Timestamp" PostgresqlTypes.TimestampSpec.spec
  describe "Timestamptz" PostgresqlTypes.TimestamptzSpec.spec
  describe "Timetz" PostgresqlTypes.TimetzSpec.spec
  describe "Uuid" PostgresqlTypes.UuidSpec.spec
  describe "Varbit" PostgresqlTypes.VarbitSpec.spec
  describe "Varchar" PostgresqlTypes.VarcharSpec.spec
