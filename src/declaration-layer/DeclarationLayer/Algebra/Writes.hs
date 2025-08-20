module DeclarationLayer.Algebra.Writes where

import DeclarationLayer.Prelude
import PtrPoker.Write

arrayHeader :: Int32 -> Bool -> Word32 -> [Int32] -> Write
arrayHeader dimensionCount elementNullable baseOid dimensions =
  mconcat
    [ bInt32 dimensionCount,
      bInt32 (if elementNullable then 1 else 0),
      bWord32 baseOid,
      foldMap arrayHeaderDimension dimensions
    ]

arrayHeaderDimension :: Int32 -> Write
arrayHeaderDimension size =
  bInt32 size <> bInt32 1
