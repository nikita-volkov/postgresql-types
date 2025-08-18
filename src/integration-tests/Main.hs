module Main (main) where

import Data.Proxy
import qualified Main.Helpers
import qualified PrimitiveLayer
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    aroundAll Main.Helpers.withPqConnection do
      Main.Helpers.primitiveSpec @PrimitiveLayer.Bool Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Bytea Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Char Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Date Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Float4 Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Float8 Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Int2 Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Int4 Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Int8 Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Inet Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Interval Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Json Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Jsonb Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Macaddr Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Numeric Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Money Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Oid Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Text Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Time Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Timestamp Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Timestamptz Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Uuid Proxy
      Main.Helpers.primitiveSpec @PrimitiveLayer.Varchar Proxy
