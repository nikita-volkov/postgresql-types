module Main (main) where

import Main.Helpers
import qualified PrimitiveLayer.Types as PrimitiveLayer
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    aroundAll withPqConnection do
      withType @PrimitiveLayer.Bit [mappingSpec]
      withType @PrimitiveLayer.Bool [mappingSpec]
      withType @PrimitiveLayer.Box [mappingSpec]
      withType @PrimitiveLayer.Bytea [mappingSpec]
      withType @PrimitiveLayer.Char [mappingSpec]
      withType @PrimitiveLayer.Cidr [mappingSpec]
      withType @PrimitiveLayer.Circle [mappingSpec]
      withType @PrimitiveLayer.Date [mappingSpec]
      withType @PrimitiveLayer.Float4 [mappingSpec]
      withType @PrimitiveLayer.Float8 [mappingSpec]
      withType @PrimitiveLayer.Inet [mappingSpec]
      withType @PrimitiveLayer.Int2 [mappingSpec]
      withType @PrimitiveLayer.Int4 [mappingSpec]
      withType @PrimitiveLayer.Int8 [mappingSpec]
      withType @PrimitiveLayer.Interval [mappingSpec]
      withType @PrimitiveLayer.IntervalAsMicroseconds [mappingSpec]
      withType @PrimitiveLayer.Json [mappingSpec]
      withType @PrimitiveLayer.Jsonb [mappingSpec]
      withType @PrimitiveLayer.Line [mappingSpec]
      withType @PrimitiveLayer.Lseg [mappingSpec]
      withType @PrimitiveLayer.Macaddr [mappingSpec]
      withType @PrimitiveLayer.Macaddr8 [mappingSpec]
      withType @PrimitiveLayer.Money [mappingSpec]
      withType @PrimitiveLayer.Numeric [mappingSpec]
      withType @PrimitiveLayer.Oid [mappingSpec]
      withType @PrimitiveLayer.Path [mappingSpec]
      withType @PrimitiveLayer.Point [mappingSpec]
      withType @PrimitiveLayer.Polygon [mappingSpec]
      withType @(PrimitiveLayer.Range PrimitiveLayer.Int4) [mappingSpec]
      withType @(PrimitiveLayer.Range PrimitiveLayer.Int8) [mappingSpec]
      withType @PrimitiveLayer.Text [mappingSpec]
      withType @PrimitiveLayer.Time [mappingSpec]
      withType @PrimitiveLayer.Timestamp [mappingSpec]
      withType @PrimitiveLayer.Timestamptz [mappingSpec]
      withType @PrimitiveLayer.Timetz [mappingSpec]
      withType @PrimitiveLayer.Uuid [mappingSpec]
      withType @PrimitiveLayer.Varbit [mappingSpec]
      withType @PrimitiveLayer.Varchar [mappingSpec]
