module QuickCheckExtras.Gen where

import qualified Data.Set as Set
import Test.QuickCheck
import Prelude

setOfSize :: (Ord a) => Int -> Gen a -> Gen (Set.Set a)
setOfSize n elementGen = go Set.empty
  where
    go currentSet
      | Set.size currentSet >= n = pure currentSet
      | otherwise = do
          newElement <- elementGen
          go (Set.insert newElement currentSet)
