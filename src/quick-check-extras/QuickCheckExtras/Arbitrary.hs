module QuickCheckExtras.Arbitrary where

import qualified QuickCheckExtras.Gen as Gen
import Test.QuickCheck
import Prelude

fromInt :: (Arbitrary a) => Int -> a
fromInt = Gen.run arbitrary 10
