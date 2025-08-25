module QuickCheckExtras.Gen where

import qualified Data.Set as Set
import System.Random (RandomGen)
import qualified System.Random as SysRandom
import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Random as Random
import Prelude
import Prelude hiding (choose, optional)

-- * Execution

run ::
  Gen a ->
  -- | Size.
  Int ->
  -- | Seed.
  Int ->
  a
run gen size seed = unGen gen (Random.mkQCGen seed) size

-- * Combinators

-- | Get the current seed from the underlying generator.
getSeed :: Gen Int
getSeed = MkGen (\qcGen _size -> 
  let (randomInt, _) = SysRandom.randomR (minBound, maxBound) qcGen
  in randomInt)

setOfSize :: (Ord a) => Int -> Gen a -> Gen (Set.Set a)
setOfSize n elementGen = go Set.empty
  where
    go currentSet
      | Set.size currentSet >= n = pure currentSet
      | otherwise = do
          newElement <- elementGen
          go (Set.insert newElement currentSet)
