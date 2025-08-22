module PrimitiveLayer.Types.Bool (Bool) where

import qualified Data.Bool
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Bool)
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @bool@ type. Logical Boolean (@true@/@false@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-boolean.html)
newtype Bool = Bool Data.Bool.Bool
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Bool)

instance Primitive Bool where
  typeName = Tagged "bool"
  baseOid = Tagged 16
  arrayOid = Tagged 1000
  binaryEncoder (Bool b) = Write.word8 (if b then 1 else 0)
  binaryDecoder =
    PeekyBlinders.statically do
      b <- PeekyBlinders.unsignedInt1
      pure (Right (Bool (b /= 0)))
  textualEncoder (Bool b) = if b then "t" else "f"

-- | Direct conversion from Haskell 'Data.Bool.Bool'.
-- This is always safe since both types represent the same values.
instance IsSome Data.Bool.Bool Bool where
  to (Bool b) = b
  maybeFrom = Just . Bool

-- | Direct conversion from PostgreSQL Bool to Haskell 'Data.Bool.Bool'.
-- This is always safe since both types represent the same values.
instance IsSome Bool Data.Bool.Bool where
  to b = Bool b
  maybeFrom (Bool b) = Just b

-- | Direct conversion from Haskell 'Data.Bool.Bool'.
-- This is a total conversion as it always succeeds.
instance IsMany Data.Bool.Bool Bool where
  from = Bool

-- | Direct conversion from PostgreSQL Bool to Haskell 'Data.Bool.Bool'.
-- This is a total conversion as it always succeeds.
instance IsMany Bool Data.Bool.Bool where
  from (Bool b) = b

-- | Bidirectional conversion between Haskell 'Data.Bool.Bool' and PostgreSQL Bool.
instance Is Data.Bool.Bool Bool

instance Is Bool Data.Bool.Bool
