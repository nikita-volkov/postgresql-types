module PostgresqlTypes.Multirange.QuickCheckGen
where

import PostgresqlTypes.Prelude
import Test.QuickCheck
import qualified Data.Set as Set

-- | Attention. This generator may run indefinitely if the 'elementGen' has too small variety of possible values.
setOfSize :: (Ord a) => Int -> Gen a -> Gen (Set.Set a)
setOfSize n elementGen = go Set.empty 0
  where
    go !currentSet !size
      | size >= n = pure currentSet
      | otherwise = do
          newElement <- suchThat elementGen \element -> Set.notMember element currentSet
          go (Set.insert newElement currentSet) (succ size)