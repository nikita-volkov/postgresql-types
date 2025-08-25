-- |
-- Refs:
-- - https://github.com/nick8325/quickcheck/blob/master/src/Test/QuickCheck/Random.hs
-- - https://hackage-content.haskell.org/package/splitmix-0.1.3.1/docs/System-Random-SplitMix.html
module QuickCheckExtras.Random where

import qualified System.Random.SplitMix
import Test.QuickCheck.Random
import Prelude

toSeed :: QCGen -> Int
toSeed (QCGen smGen) =
  let (seed, _gamma) = System.Random.SplitMix.unseedSMGen smGen
   in fromIntegral seed
