module Main (main) where

import Data.Proxy
import qualified Main.Helpers
import qualified PrimitiveLayer.Types as PrimitiveLayer
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    aroundAll Main.Helpers.withPqConnection do
      Main.Helpers.mappingSpec @PrimitiveLayer.Bit Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Bool Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Box Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Bytea Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Char Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Cidr Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Circle Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Date Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Float4 Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Float8 Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Inet Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Int2 Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Int4 Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Int8 Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Interval Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.IntervalAsMicroseconds Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Json Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Jsonb Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Line Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Lseg Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Macaddr Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Macaddr8 Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Money Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Numeric Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Oid Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Path Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Point Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Polygon Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Text Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Time Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Timestamp Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Timestamptz Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Timetz Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Uuid Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Varbit Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Varchar Proxy
