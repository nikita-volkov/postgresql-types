module PrimitiveLayer.Vias.Primitive where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude

newtype ViaPrimitive a = ViaPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, Primitive)

instance (Primitive a) => Show (ViaPrimitive a) where
  showsPrec d (ViaPrimitive a) = showsPrec d (textualEncoder a)
