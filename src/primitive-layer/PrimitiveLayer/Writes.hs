-- | Patterns for writing PostgreSQL values.
module PrimitiveLayer.Writes where

import PrimitiveLayer.Prelude
import PtrPoker.Write

-- | Prefix a write with its size.
sized :: Write -> Write
sized write =
  bWord32 (fromIntegral (writeSize write)) <> write
