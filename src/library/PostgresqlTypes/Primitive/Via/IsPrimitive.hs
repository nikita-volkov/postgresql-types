module PostgresqlTypes.Primitive.Via.IsPrimitive where

import PostgresqlTypes.Primitive.Algebra
import PostgresqlTypes.Primitive.Prelude

newtype ViaIsPrimitive a = ViaIsPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, IsPrimitive)

instance (IsPrimitive a) => Show (ViaIsPrimitive a) where
  showsPrec d (ViaIsPrimitive a) = showsPrec d (textualEncoder a)
