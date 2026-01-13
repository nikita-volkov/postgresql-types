module PostgresqlTypes.Codec.IsComposite where

import PostgresqlTypes.Codec.Fields as Fields
import PostgresqlTypes.Codec.Prelude
import PostgresqlTypes.Codec.Scalar

-- | Composite type mapping.
class IsComposite a where
  compositeSchema :: Tagged a Text
  compositeName :: Tagged a Text
  compositeFields :: Fields a a

composite :: forall a. (IsComposite a) => Scalar a
composite =
  Fields.composite
    (untag (compositeSchema @a))
    (untag (compositeName @a))
    compositeFields
