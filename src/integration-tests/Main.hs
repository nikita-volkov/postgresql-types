module Main (main) where

import qualified Main.Helpers
import qualified PrimitiveLayer
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    aroundAll Main.Helpers.withPqConnection do
      Main.Helpers.mappingSpec @PrimitiveLayer.UUID PrimitiveLayer.mapping
      Main.Helpers.mappingSpec @PrimitiveLayer.Jsonb PrimitiveLayer.mapping
      Main.Helpers.mappingSpec @PrimitiveLayer.Macaddr PrimitiveLayer.mapping
