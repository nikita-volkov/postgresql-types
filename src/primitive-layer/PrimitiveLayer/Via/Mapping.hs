module PrimitiveLayer.Via.Mapping where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude

newtype ViaPrimitive a = ViaPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, Mapping)

instance (Mapping a) => Show (ViaPrimitive a) where
  showsPrec d (ViaPrimitive a) = showsPrec d (textualEncoder a)
