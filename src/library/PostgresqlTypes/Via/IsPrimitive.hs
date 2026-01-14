module PostgresqlTypes.Via.IsPrimitive where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude

newtype ViaIsPrimitive a = ViaIsPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, IsPrimitive)

instance (IsPrimitive a) => Show (ViaIsPrimitive a) where
  showsPrec d (ViaIsPrimitive a) = showsPrec d (textualEncoder a)
