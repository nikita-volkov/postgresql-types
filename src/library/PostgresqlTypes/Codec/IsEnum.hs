module PostgresqlTypes.Codec.IsEnum where

import PostgresqlTypes.Codec.Prelude
import PostgresqlTypes.Codec.Scalar as Scalar

enum :: forall a. (IsEnum a) => Scalar a
enum =
  Scalar.enum
    (untag (enumSchema @a))
    (untag (enumName @a))
    (enumVariants @a)

-- | Enumeration type mapping.
class (Ord a) => IsEnum a where
  enumSchema :: Tagged a Text
  enumName :: Tagged a Text

  -- | List of Postgres enumeration label to Haskell value associations.
  enumVariants :: [(Text, a)]
