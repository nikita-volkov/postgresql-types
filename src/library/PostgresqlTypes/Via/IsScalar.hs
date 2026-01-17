module PostgresqlTypes.Via.IsScalar where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude

newtype ViaIsScalar a = ViaIsScalar a
  deriving newtype (Eq, Ord, Arbitrary, IsScalar)

instance (IsScalar a) => Show (ViaIsScalar a) where
  showsPrec d (ViaIsScalar a) = showsPrec d (textualEncoder a)
