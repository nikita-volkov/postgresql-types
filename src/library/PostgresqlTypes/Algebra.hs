module PostgresqlTypes.Algebra where

import qualified PeekyBlinders
import PostgresqlTypes.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

class PostgresqlType a where
  name :: Tagged a Text
  baseOid :: Tagged a Int32
  arrayOid :: Tagged a Int32
  binaryEncoder :: a -> Write.Write
  binaryDecoder :: PeekyBlinders.Dynamic a

  -- | Represent in Postgres textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder
