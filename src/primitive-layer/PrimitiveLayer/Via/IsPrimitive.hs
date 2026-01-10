module PrimitiveLayer.Via.IsPrimitive where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude

newtype ViaIsPrimitive a = ViaIsPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, IsPrimitive)

instance (IsPrimitive a) => Show (ViaIsPrimitive a) where
  showsPrec d (ViaIsPrimitive a) = showsPrec d (textualEncoder a)
