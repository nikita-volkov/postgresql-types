module PostgresqlTypes.Codec.Result where

import PostgresqlTypes.Codec.Columns
import PostgresqlTypes.Codec.Prelude

-- | Result decoder.
data Result a
  = SingleRow (Columns a)
  | forall row. Multirow (Columns row) (Vector row -> a)
  | RowsAffected (Int -> a)

single :: Columns row -> Result row
single = error "TODO"

multirow :: Columns row -> Result (Vector row)
multirow = error "TODO"

rowsAffected :: Result Int
rowsAffected = error "TODO"
