module PrimitiveLayer.Types.Boolean (Boolean) where

import qualified Data.Bool
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @boolean@ type. Logical Boolean (@true@/@false@).
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-boolean.html)
newtype Boolean = Boolean Data.Bool.Bool
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Boolean)

instance Mapping Boolean where
  typeName = Tagged "boolean"
  baseOid = Tagged 16
  arrayOid = Tagged 1000
  binaryEncoder (Boolean b) = Write.word8 (if b then 1 else 0)
  binaryDecoder =
    PeekyBlinders.statically do
      b <- PeekyBlinders.unsignedInt1
      pure (Right (Boolean (b /= 0)))
  textualEncoder (Boolean b) = if b then "t" else "f"

-- | Direct conversion from Haskell 'Data.Bool.Bool'.
-- This is always safe since both types represent the same values.
instance IsSome Data.Bool.Bool Boolean where
  to (Boolean b) = b
  maybeFrom = Just . Boolean

-- | Direct conversion from PostgreSQL Boolean to Haskell 'Data.Bool.Bool'.
-- This is always safe since both types represent the same values.
instance IsSome Boolean Data.Bool.Bool where
  to b = Boolean b
  maybeFrom (Boolean b) = Just b

-- | Direct conversion from Haskell 'Data.Bool.Bool'.
-- This is a total conversion as it always succeeds.
instance IsMany Data.Bool.Bool Boolean where
  from = Boolean

-- | Direct conversion from PostgreSQL Boolean to Haskell 'Data.Bool.Bool'.
-- This is a total conversion as it always succeeds.
instance IsMany Boolean Data.Bool.Bool where
  from (Boolean b) = b

-- | Bidirectional conversion between Haskell 'Data.Bool.Bool' and PostgreSQL Boolean.
instance Is Data.Bool.Bool Boolean

instance Is Boolean Data.Bool.Bool
