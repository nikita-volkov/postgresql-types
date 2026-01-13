module PostgresqlTypes.Codec.IsEnum where

import PostgresqlTypes.Codec.Prelude
import PostgresqlTypes.Codec.Scalar

enum :: (IsEnum a) => Scalar a
enum =
  error "TODO"

-- | Enumeration type mapping.
class IsEnum a where
  enumSchema :: Tagged a Text
  enumName :: Tagged a Text

  -- | List of Postgres enumeration label to Haskell value associations.
  enumVariants :: [(Text, a)]
