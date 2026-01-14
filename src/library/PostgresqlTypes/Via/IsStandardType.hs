module PostgresqlTypes.Via.IsStandardType where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude

newtype ViaIsStandardType a = ViaIsStandardType a
  deriving newtype (Eq, Ord, Arbitrary, IsStandardType)

instance (IsStandardType a) => Show (ViaIsStandardType a) where
  showsPrec d (ViaIsStandardType a) = showsPrec d (textualEncoder a)
