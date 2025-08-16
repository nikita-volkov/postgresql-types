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
      Main.Helpers.mappingSpec @PrimitiveLayer.UUID Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Jsonb Proxy
      Main.Helpers.mappingSpec @PrimitiveLayer.Macaddr Proxy
