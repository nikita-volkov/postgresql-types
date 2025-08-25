module QuickCheckExtras.Gen where

import Data.Function
import qualified Data.Set as Set
import qualified QuickCheckExtras.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Random as Random
import Prelude hiding (filter)

-- * Execution

-- | Execute the QuickCheck generator deterministically.
run ::
  Gen a ->
  -- | Size.
  Int ->
  -- | Seed.
  Int ->
  a
run gen size seed = unGen gen (Random.mkQCGen seed) size

-- * Combinators

filter :: (a -> Bool) -> Gen a -> Gen a
filter = flip suchThat

setOfSize :: (Ord a) => Int -> Gen a -> Gen (Set.Set a)
setOfSize n elementGen = go Set.empty 0
  where
    go !currentSet !size
      | size >= n = pure currentSet
      | otherwise = do
          newElement <- suchThat elementGen \element -> Set.notMember element currentSet
          go (Set.insert newElement currentSet) (succ size)

-- | Get the current seed from the underlying generator.
getSeed :: Gen Int
getSeed =
  MkGen \rnd _size ->
    QuickCheckExtras.Random.toSeed rnd
