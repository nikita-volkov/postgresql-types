module QuickCheckExtras.Gen where

import qualified Data.Set as Set
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

setOfSize :: (Ord a) => Int -> Gen a -> Gen (Set.Set a)
setOfSize n elementGen = go Set.empty
  where
    go currentSet
      | Set.size currentSet >= n = pure currentSet
      | otherwise = do
          newElement <- elementGen
          go (Set.insert newElement currentSet)
