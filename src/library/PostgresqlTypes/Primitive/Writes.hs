-- | Patterns for writing PostgreSQL values.
module PostgresqlTypes.Primitive.Writes where

import PostgresqlTypes.Primitive.Prelude
import PtrPoker.Write

-- | Prefix a write with its size.
sized :: Write -> Write
sized write =
  bWord32 (fromIntegral (writeSize write)) <> write
