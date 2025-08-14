module Main (main) where

import qualified Main.Helpers
import qualified PostgresqlTypes
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    aroundAll Main.Helpers.withPqConnection do
      Main.Helpers.mappingSpec @PostgresqlTypes.UUID PostgresqlTypes.mapping
      Main.Helpers.mappingSpec @PostgresqlTypes.Jsonb PostgresqlTypes.mapping
      Main.Helpers.mappingSpec @PostgresqlTypes.Macaddr PostgresqlTypes.mapping
